module Main where

import Prelude

import Components.TodoListe as TodoListe
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  runHalogenAff do
    -- get a handle to the body after it's available
    body <- awaitBody
    -- run the game-component with the random game as input
    -- into the body of the browser-document
    runUI TodoListe.component unit body