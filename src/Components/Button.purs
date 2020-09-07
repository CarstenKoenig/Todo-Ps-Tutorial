module Components.Button where


import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen (Component)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as Ev
import Halogen.Hooks as Hooks

type Query = Const Void
type Input = Int
type Output = Void

button :: forall m . Component HTML Query Input Output m
button = Hooks.component \{ } intitalCount -> Hooks.do
  -- Declare a new state variable, which we'll call "count"
  count /\ countId <- Hooks.useState intitalCount

  Hooks.pure do
    H.div_
      [ H.p_ [ H.text $ "You clicked " <> show count <> " times" ]
      , H.button
          [ Ev.onClick \_ -> Just $ Hooks.modify_ countId (_ + 1) ]
          [ H.text "Click me" ]
      ]