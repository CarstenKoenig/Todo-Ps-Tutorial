module Models.Todo 
    ( Todo
    , Index
    , List
    , toggleCompleted
    , toggleAll
    , emptyList
    , insert
    , update
    , delete
    , items
    ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Data.Foldable (any)


type Todo =
    { text :: String
    , completed :: Boolean
    }

toggleCompleted :: Todo -> Todo
toggleCompleted todo =
    todo { completed = not todo.completed }


type Index = Int

newtype List 
    = List (Map Index Todo)

derive instance newtypeList :: Newtype List _

emptyList :: List
emptyList = List Map.empty

insert :: Todo -> List -> List
insert toDo (List toDoList) =
    List $ Map.insert newIndex toDo toDoList
    where
    newIndex = maybe 0 (\{ key } -> key+1) $ Map.findMax toDoList 

update :: Index -> (Todo -> Todo) -> List -> List
update index upd =
    over List $ Map.update (upd >>> Just) index
    
delete :: Index -> List -> List
delete index = 
    over List (Map.delete index)

items :: forall f. Functor f => Unfoldable f => List -> f { index :: Index, item :: Todo }
items (List todos) = 
    map (\(Tuple index item) -> { index, item }) $ Map.toUnfoldable todos

toggleAll :: List -> List
toggleAll (List todos) = 
    List (map setCompleted todos)
    where
    setCompleted :: Todo -> Todo
    setCompleted = _ { completed = anyActiveTodosLeft }
    anyActiveTodosLeft = any (not <<< _.completed) todos