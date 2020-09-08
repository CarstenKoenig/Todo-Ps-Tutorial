module Main where

import Prelude

import Components.TodoListe as TodoListe
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Routing.Hash (matches)
import Routes (Route, route)
import Data.Maybe (Maybe (..))

main :: Effect Unit
main = do
  runHalogenAff do
    -- get a handle to the body after it's available
    body <- awaitBody
    -- run the game-component with the random game as input
    -- into the body of the browser-document
    io <- runUI TodoListe.component unit body
    _ <- liftEffect $ matches route (handleRouteChange io.query) 

    pure unit

handleRouteChange :: (forall a. TodoListe.Query a -> Aff (Maybe a)) -> Maybe (Maybe Route) -> Maybe Route -> Effect Unit
handleRouteChange query _ (Just newRoute) =
  launchAff_ $ query $ H.tell (TodoListe.RouteChanged newRoute)
handleRouteChange _ _ Nothing =
  pure unit