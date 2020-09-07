{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "HalogenIntro"
, dependencies =
  [ "arrays", "console", "effect", "halogen-hooks", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
