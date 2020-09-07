module Components.TodoItem where


import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component, RefLabel(..), liftEffect)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as Ev
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as Attr
import Halogen.Hooks as Hooks
import Models.Todo (Todo, Index)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.MouseEvent (toEvent)

type Query = Const Void
type Input = { index :: Index, item :: Todo }
data Output 
    = ToggleCompleted Index
    | RemoveToDo Index
    | UpdateText Index String

component :: forall m . MonadEffect m => Component HTML Query Input Output m
component = Hooks.component comp
  where
  comp { outputToken } todo = Hooks.do
    let inputRef = RefLabel ("input_" <> show todo.index)
    editing /\ editingId <- Hooks.useState Nothing

    Hooks.captures {} Hooks.useTickEffect do
        element <- Hooks.getHTMLElementRef inputRef
        liftEffect $ maybe (pure unit) focus element
        pure Nothing

    Hooks.pure do
        case editing of 
            Nothing ->
                H.li_
                    [ H.div
                        [ Attr.class_ (ClassName "view")]
                        [ H.input
                            [ Attr.class_ (ClassName "toggle") 
                            , Attr.type_ InputCheckbox
                            , Attr.checked todo.item.completed
                            , Ev.onClick onClickHandler
                            ]
                        , H.label
                            [ Ev.onDoubleClick (onDoubleClickHandler editingId)
                            , Attr.classes (if todo.item.completed then [ ClassName "completed"] else [])
                            ]
                            [ H.text todo.item.text ]
                        , H.button 
                            [ Attr.class_ (ClassName "destroy")
                            , Ev.onClick onClickRemoveHandler 
                            ]
                            []
                        ]
                    ]
            Just txt -> 
                H.li
                    [ Attr.class_ (ClassName "editing") ]
                    [ H.form 
                        [ Ev.onSubmit (onEditingDone txt editingId) ]
                        [ H.input
                            [ Attr.class_ (ClassName "edit") 
                            , Attr.value txt 
                            , Attr.autofocus true
                            , Attr.ref inputRef
                            , Ev.onValueInput (\newTxt -> Just $ Hooks.put editingId (Just newTxt))
                            , Ev.onBlur (leaveInputField editingId)
                            ]
                        ]
                    ]
    where
    onEditingDone newText editingId ev = Just do
        liftEffect $ preventDefault ev
        Hooks.put editingId Nothing
        Hooks.raise outputToken (UpdateText todo.index newText)
    onDoubleClickHandler editingId mouseEvent = Just do
        liftEffect $ preventDefault $ toEvent mouseEvent
        Hooks.put editingId (Just todo.item.text)
    onClickHandler _ = Just $
        Hooks.raise outputToken (ToggleCompleted todo.index)
    onClickRemoveHandler _ = Just $
        Hooks.raise outputToken (RemoveToDo todo.index)
    leaveInputField editingId _ = Just do 
        Hooks.put editingId Nothing
