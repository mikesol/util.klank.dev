module Klank.Dev.Util where

import Prelude
import Control.Promise (toAffE)
import Data.Array (filter)
import Data.Either (Either(..), either)
import Data.Lens (_2, over)
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
import Type.Klank.Dev (Buffers, Images, Videos)
import Type.Row.Homogeneous (class Homogeneous)
import Web.DOM.Document (createElement)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (HTMLImageElement, HTMLVideoElement, window)
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

fetchVideo :: String -> Aff HTMLVideoElement
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

fetchImage :: String -> Aff HTMLImageElement
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

loopDownload :: forall a. (String -> Aff a) -> Int -> String -> Aff a
loopDownload f maxAttempts str = go 0
  where
  go nt =
    res
      >>= either
          ( \e ->
              if nt > maxAttempts then
                throwError (error $ "Max download attempts reached for " <> str)
              else do
                delay (Milliseconds 20.0)
                go (nt + 1)
          )
          pure
    where
    res = try $ f str

makeSomethingUsingCache :: forall a. (String -> Aff a) -> Int -> (O.Object a -> Tuple (Array (Tuple String String)) (O.Object a)) -> O.Object a -> (O.Object a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
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

type CacheFunction a
  = O.Object a -> Tuple (Array (Tuple String String)) (O.Object a)

makeBuffersUsingCache :: Int -> CacheFunction BrowserAudioBuffer -> Buffers
makeBuffersUsingCache maxAttempts bf ctx = makeSomethingUsingCache (toAffE <<< decodeAudioDataFromUri ctx) maxAttempts bf

makeBuffersKeepingCache :: Int -> Array (Tuple String String) -> Buffers
makeBuffersKeepingCache maxAttempts = (makeBuffersUsingCache maxAttempts) <<< Tuple

makeImagesUsingCache :: Int -> CacheFunction HTMLImageElement -> Images
makeImagesUsingCache = makeSomethingUsingCache fetchImage

makeImagesKeepingCache :: Int -> Array (Tuple String String) -> Images
makeImagesKeepingCache maxAttempts = (makeImagesUsingCache maxAttempts) <<< Tuple

makeVideosUsingCache :: Int -> CacheFunction HTMLVideoElement -> Videos
makeVideosUsingCache = makeSomethingUsingCache fetchVideo

makeVideosKeepingCache :: Int -> Array (Tuple String String) -> Videos
makeVideosKeepingCache maxAttempts = (makeVideosUsingCache maxAttempts) <<< Tuple

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
