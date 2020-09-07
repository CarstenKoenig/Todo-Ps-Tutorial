module Components.TodoListe where


import Prelude

import Components.TodoItem as TodoItem
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component, Slot, liftEffect)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as Ev
import Halogen.HTML.Properties as Attr
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Models.Todo (Index, Todo)
import Models.Todo as Todo
import Web.Event.Event (Event, preventDefault)

type Query = Const Void
type Input = Unit
type Output = Void

type Slots =
    { items :: Slot TodoItem.Query TodoItem.Input Todo.Index }

_items = SProxy :: SProxy "items"

component :: forall m . MonadEffect m => Component HTML Query Input Output m
component = Hooks.component \{ slotToken } _ -> Hooks.do
  todos /\ todosId <- Hooks.useState Todo.emptyList
  newTodoText /\ newTodoTextId <- Hooks.useState ""

  Hooks.pure $ view { todos, todosId, newTodoText, newTodoTextId } 
  where
    view states = do
        H.div_
            [ viewInput
            , viewTodoList
            ]
        where
        viewInput = 
            H.form
                [ Ev.onSubmit handleNewTodoSubmit ]
                [ H.input
                    [ Attr.placeholder "new todo..."
                    , Attr.value states.newTodoText
                    , Ev.onValueInput handleNewTodoTextChanged
                    ]
                ]
        viewTodoList =
            H.ul
                [ Attr.class_ (ClassName "todo-list") ]
                (map viewTodoItem $ Todo.items states.todos)
        viewTodoItem :: { index :: Index, item :: Todo } -> _
        viewTodoItem todo =
            H.li_
                [ H.slot _items todo.index TodoItem.component todo handleTodoItemOutput
                ]
        handleNewTodoSubmit :: Event -> Maybe (HookM m Unit)
        handleNewTodoSubmit ev = Just $ do
            liftEffect $ preventDefault ev
            Hooks.modify_ states.todosId (Todo.insert { text: states.newTodoText, completed: false })
            Hooks.put states.newTodoTextId ""
            pure unit
        handleNewTodoTextChanged newText = 
            Just $ Hooks.put states.newTodoTextId newText
        handleTodoItemOutput itemOutput =
            case itemOutput of
                TodoItem.ToggleCompleted index ->
                    Just $ Hooks.modify_ states.todosId (Todo.update index Todo.toggleCompleted)
                TodoItem.RemoveToDo index -> 
                    Just $ Hooks.modify_ states.todosId (Todo.delete index)
                TodoItem.UpdateText index txt ->
                    Just $ Hooks.modify_ states.todosId (Todo.update index _{ text = txt })