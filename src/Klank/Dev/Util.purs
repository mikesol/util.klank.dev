module Klank.Dev.Util where

import Prelude
import Control.Promise (toAffE)
import Data.Array (filter)
import Data.Either (Either(..), either)
import Data.Lens (_2, over)
import Data.List (List)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, launchAff_, makeAff, parallel, sequential, throwError, try)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import FRP.Behavior.Audio (BrowserAudioBuffer, decodeAudioDataFromUri)
import Foreign.Object (Object, fromHomogeneous)
import Foreign.Object as O
import Graphics.Canvas (CanvasElement, TextMetrics, getContext2D)
import Graphics.Painting (MeasurableText, Painting, render, measurableTextToMetrics)
import Type.Klank.Dev (Buffers, Images, Videos, Canvases)
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (createElement)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLImageElement as HTMLImageElement
import Web.HTML.HTMLMediaElement as HTMLMediaElement
import Web.HTML.HTMLVideoElement as HTMLVideoElement
import Web.HTML.Window (document)

affize :: forall a. ((a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit) -> Aff a
affize f =
  makeAff \cb -> do
    f (cb <<< Right) (cb <<< Left)
    pure mempty

fetchVideo :: String -> Aff HTMLVideoElement.HTMLVideoElement
fetchVideo str =
  affize \res rej -> do
    document <- (toDocument <$> (document =<< window))
    node <- HTMLVideoElement.fromElement <$> (createElement "video" document)
    video <- case node of
      Nothing -> throwError (error "Could not convert to video node")
      Just x -> pure x
    HTMLMediaElement.setCrossOrigin "anonymous" (HTMLVideoElement.toHTMLMediaElement video)
    videoListener <- eventListener \e -> res video
    addEventListener
      (EventType "loadeddata")
      videoListener
      false
      (HTMLVideoElement.toEventTarget video)
    errorListener <-
      eventListener \e ->
        rej $ error "Could not load video"
    addEventListener
      (EventType "error")
      errorListener
      false
      (HTMLVideoElement.toEventTarget video)
    HTMLMediaElement.setSrc str (HTMLVideoElement.toHTMLMediaElement video)

fetchCanvas :: CanvasInfo -> Aff HTMLCanvasElement.HTMLCanvasElement
fetchCanvas ci = do
  images <- affize \res rej -> makeImagesKeepingCache 20 ci.images O.empty res rej
  videos <- affize \res rej -> makeVideosKeepingCache 20 ci.videos O.empty res rej
  liftEffect do
    document <- (toDocument <$> (document =<< window))
    node <- HTMLCanvasElement.fromElement <$> (createElement "canvas" document)
    canvas <- case node of
      Nothing -> throwError (error "Could not convert to canvas node")
      Just x -> pure x
    HTMLCanvasElement.setWidth ci.width canvas
    HTMLCanvasElement.setHeight ci.height canvas
    canvasCtx <- getContext2D $ htmlCanvasElementToCanvasElement canvas
    words <- measurableTextToMetrics canvasCtx ci.words
    let
      painting = ci.painting { words }
    render canvasCtx
      { canvases: O.empty
      , images
      , videos
      }
      painting
    pure canvas

fetchImage :: String -> Aff HTMLImageElement.HTMLImageElement
fetchImage str =
  affize \res rej -> do
    image <- HTMLImageElement.create unit
    HTMLImageElement.setCrossOrigin "anonymous" image
    imageListener <- eventListener \e -> res image
    addEventListener
      (EventType "load")
      imageListener
      false
      (HTMLImageElement.toEventTarget image)
    errorListener <-
      eventListener \e ->
        rej $ error "Could not load video"
    addEventListener
      (EventType "error")
      errorListener
      false
      (HTMLImageElement.toEventTarget image)
    HTMLImageElement.setSrc str image

loopDownload :: forall a b. (b -> Aff a) -> Int -> b -> Aff a
loopDownload f maxAttempts str = go 0
  where
  go nt =
    res
      >>= either
          ( \e ->
              if nt > maxAttempts then
                throwError (error $ "Max fetching attempts reached for asset.")
              else do
                delay (Milliseconds 20.0)
                go (nt + 1)
          )
          pure
    where
    res = try $ f str

makeSomethingUsingCache :: forall a b. (b -> Aff a) -> Int -> (O.Object a -> Tuple (Array (Tuple String b)) (O.Object a)) -> O.Object a -> (O.Object a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
makeSomethingUsingCache looper maxAttempts bf prev' =
  affable do
    sequential
      ( O.union <$> (pure prev)
          <*> ( sequence
                $ O.fromFoldable
                    ( map
                        ( over _2
                            (parallel <<< loopDownload looper maxAttempts)
                        )
                        (filter (not <<< flip O.member prev <<< fst) newB)
                    )
            )
      )
  where
  (Tuple newB prev) = bf prev'

type CacheFunction a b
  = O.Object a -> Tuple (Array (Tuple String b)) (O.Object a)

makeBuffersUsingCache :: Int -> CacheFunction BrowserAudioBuffer String -> Buffers
makeBuffersUsingCache maxAttempts bf ctx = makeSomethingUsingCache (toAffE <<< decodeAudioDataFromUri ctx) maxAttempts bf

makeBuffersKeepingCache :: Int -> Array (Tuple String String) -> Buffers
makeBuffersKeepingCache maxAttempts = (makeBuffersUsingCache maxAttempts) <<< Tuple

makeImagesUsingCache :: Int -> CacheFunction HTMLImageElement.HTMLImageElement String -> Images
makeImagesUsingCache = makeSomethingUsingCache fetchImage

makeImagesKeepingCache :: Int -> Array (Tuple String String) -> Images
makeImagesKeepingCache maxAttempts = (makeImagesUsingCache maxAttempts) <<< Tuple

makeVideosUsingCache :: Int -> CacheFunction HTMLVideoElement.HTMLVideoElement String -> Videos
makeVideosUsingCache = makeSomethingUsingCache fetchVideo

makeVideosKeepingCache :: Int -> Array (Tuple String String) -> Videos
makeVideosKeepingCache maxAttempts = (makeVideosUsingCache maxAttempts) <<< Tuple

makeCanvasesUsingCache :: Int -> CacheFunction HTMLCanvasElement.HTMLCanvasElement CanvasInfo -> Canvases
makeCanvasesUsingCache = makeSomethingUsingCache fetchCanvas

makeCanvasesKeepingCache :: Int -> Array (Tuple String CanvasInfo) -> Canvases
makeCanvasesKeepingCache maxAttempts = (makeCanvasesUsingCache maxAttempts) <<< Tuple

type CanvasInfo
  = { images :: Array (Tuple String String)
    , videos :: Array (Tuple String String)
    , painting :: { words :: M.Map MeasurableText TextMetrics } -> Painting
    , words :: List MeasurableText
    , width :: Int
    , height :: Int
    }

htmlCanvasElementToCanvasElement :: HTMLCanvasElement.HTMLCanvasElement -> CanvasElement
htmlCanvasElementToCanvasElement = unsafeCoerce

affableRec ::
  forall (a :: # Type) b.
  Homogeneous a b =>
  Aff (Record a) ->
  (Object b -> Effect Unit) ->
  (Error -> Effect Unit) ->
  Effect Unit
affableRec aff res rej =
  launchAff_ do
    result <- try $ aff
    case result of
      Left err -> liftEffect $ rej err
      Right resp -> liftEffect $ res (fromHomogeneous resp)

affable :: forall a. Aff a -> (a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
affable aff res rej =
  launchAff_ do
    result <- try $ aff
    case result of
      Left err -> liftEffect $ rej err
      Right resp -> liftEffect $ res resp
