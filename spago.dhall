{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "HalogenIntro"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "generics-rep"
  , "halogen-hooks"
  , "psci-support"
  , "routing"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
