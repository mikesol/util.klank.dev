{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "klank-dev-util"
, dependencies =
  [ "audio-behaviors"
  , "console"
  , "effect"
  , "psci-support"
  , "typelevel-klank-dev"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
