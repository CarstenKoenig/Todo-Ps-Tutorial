module Components.TodoListe where


import Prelude

import Components.TodoItem as TodoItem
import Data.Array (filter)
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component, Slot, liftEffect)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as H
import Halogen.HTML.Events as Ev
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as Attr
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Models.Todo (Index, Todo)
import Models.Todo as Todo
import Routes (Route(..))
import Web.Event.Event (Event, preventDefault)

data Query cont 
    = RouteChanged Route cont

type Input = Unit
type Output = Void

type Slots =
    { items :: Slot TodoItem.Query TodoItem.Input Todo.Index }

_items = SProxy :: SProxy "items"

component :: forall m . MonadEffect m => Component HTML Query Input Output m
component = Hooks.component \{ slotToken, queryToken } _ -> Hooks.do
  todos /\ todosId <- Hooks.useState Todo.emptyList
  currentRoute /\ currentRouteId <- Hooks.useState AllTodosRoute
  newTodoText /\ newTodoTextId <- Hooks.useState ""

  Hooks.useQuery queryToken
    case _ of
        RouteChanged newRoute cont -> do
            Hooks.put currentRouteId newRoute
            pure (Just cont)


  Hooks.pure $ view { todos, todosId, newTodoText, newTodoTextId, currentRoute } 
  where
    view states = do
        H.section
            [ Attr.class_ (ClassName "todoapp") ]
            [ H.header
                [ Attr.class_ (ClassName "header") ]
                [ H.h1_ [ H.text "todos" ]
                , viewInput
                ]
            , H.section
                [ Attr.class_ (ClassName "main") ]
                [ H.input
                    [ Attr.class_ (ClassName "toggle-all")
                    , Attr.id_ "toggle-all"
                    , Attr.type_ InputCheckbox
                    , Ev.onClick handleToggleAll
                    ]
                , H.label
                    [ Attr.for "toggle-all"
                    ]
                    [ H.text "Mark all as complete" ]
                , viewTodoList 
                ]
            , viewFooter
            ]
        where
        allTodoItems = 
            Todo.items states.todos
        filteredTodoItems = 
            allTodoItems
            # filter (filterByRoute states.currentRoute)
            where
            filterByRoute AllTodosRoute _ = true
            filterByRoute CompletedTodosRoute toDo = toDo.item.completed
            filterByRoute ActiveTodosRoute toDo = not toDo.item.completed
        itemsLeft :: Int
        itemsLeft =
            length $ filteredTodoItems
        viewInput = 
            H.form
                [ Attr.class_ (ClassName "new-todo-form") 
                , Ev.onSubmit handleNewTodoSubmit ]
                [ H.input
                    [ Attr.class_ (ClassName "new-todo")
                    , Attr.placeholder "new todo..."
                    , Attr.value states.newTodoText
                    , Ev.onValueInput handleNewTodoTextChanged
                    ]
                ]
        viewTodoList =
            H.ul
                [ Attr.class_ (ClassName "todo-list") ]
                (map viewTodoItem filteredTodoItems)
        viewFooter =
            H.footer
                [ Attr.class_ (ClassName "footer") ]
                [ H.span
                    [ Attr.class_ (ClassName "todo-count") ]
                    [ H.strong_ [ H.text (show itemsLeft) ]
                    , H.text (if length filteredTodoItems > 1 then " items left" else " item left")
                    ]
                , H.ul
                    [ Attr.class_ (ClassName "filters") ]
                    [ H.li_
                        [ H.a
                            [ selected AllTodosRoute
                            , Attr.href "#/"
                            ]
                            [ H.text "All" ]
                        ]
                    , H.li_
                        [ H.a
                            [ selected ActiveTodosRoute
                            , Attr.href "#/active"
                            ]
                            [ H.text "Active" ]
                        ]
                    , H.li_
                        [ H.a
                            [ selected CompletedTodosRoute
                            , Attr.href "#/completed"
                            ]
                            [ H.text "Completed" ]
                        ]
                    ]
                ]
            where
            selected :: forall r i. Route -> IProp ( class :: String | r ) i
            selected route
                | route == states.currentRoute =
                    Attr.classes [ ClassName "selected" ]
                | otherwise =
                    Attr.classes []
        viewTodoItem :: { index :: Index, item :: Todo } -> _
        viewTodoItem todo =
            H.slot _items todo.index TodoItem.component todo handleTodoItemOutput
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
        handleToggleAll ev = Just $
            Hooks.modify_ states.todosId Todo.toggleAll