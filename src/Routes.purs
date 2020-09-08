module Routes where

import Prelude
import Data.Foldable (oneOf)
import Routing.Match (Match, root, lit, end)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

data Route
  = AllTodosRoute
  | CompletedTodosRoute
  | ActiveTodosRoute

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
    show = genericShow

route :: Match (Maybe Route)
route = root *> oneOf
  [ Just <$> (AllTodosRoute <$ end)
  , Just <$> (CompletedTodosRoute <$ lit "completed")
  , Just <$> (ActiveTodosRoute <$ lit "active")
  , pure Nothing
  ]