module Main exposing (..)

import Browser exposing (sandbox)
import Css exposing (..)
import Debug exposing (todo)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, placeholder, readonly, src, style, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)


type alias Todo =
    { text : String
    , completed : Bool
    , editable : Bool
    }


type Msg
    = AddTodo
    | RemoveTodo Int
    | EditTodo Int
    | SaveEditTodo Int String
    | ToggleTodo Int
    | ChangeInput String
    | NoOp


type alias Model =
    { todos : List Todo
    , inputText : String
    }


theme : { secondary : Color, primary : Color }
theme =
    { primary = hex "c7c7c7"
    , secondary = rgb 250 240 230
    }


fonts : { headingFont : Float, baseFont : Float, extraFont : Float }
fonts =
    { headingFont = 1.5
    , baseFont = 1
    , extraFont = 2
    }


view : Model -> Html Msg
view model =
    form
        [ onSubmit AddTodo
        , css
            [ display inlineBlock
            , margin (px 0)
            , width (vw 100)
            , height (vh 100)
            , backgroundColor theme.primary
            , fontFamilies [ "Courier New" ]
            , fontSize (rem fonts.headingFont)
            ]
        ]
        [ h1
            [ css
                [ textAlign center
                , textDecoration underline
                ]
            ]
            [ text "Your todo tasks are" ]
        , div
            [ css
                [ marginRight auto
                , marginLeft auto
                , display block
                , width (vw 80)
                ]
            ]
            [ input
                [ value model.inputText
                , onInput ChangeInput
                , placeholder "What do you want to do?"
                , css
                    [ width (vw 65)
                    , borderRadius (px 5)
                    , marginRight (px 10)
                    , fontFamilies [ "Courier New" ]
                    , fontSize (rem fonts.headingFont)
                    ]
                , Html.Styled.Attributes.required True
                ]
                []
            , button
                [ type_ "submit"
                , css
                    [ fontFamilies [ "Courier New" ]
                    , fontSize (rem fonts.headingFont)
                    , borderRadius (px 5)
                    ]
                ]
                [ text "Add" ]
            ]
        , if List.isEmpty model.todos then
            p [ css [ textAlign center ] ] [ text "How come you have no tasks? 🤔" ]

          else
            ul [ css [ listStyle none ], style "padding-inline-start" "0px"] (List.indexedMap viewTodo model.todos)
        ]


viewTodo : Int -> Todo -> Html Msg
viewTodo index todo =
    div
        [ css
            [ (if todo.completed then
                textDecoration lineThrough

              else
                textDecoration none)
                , maxWidth maxContent
                , marginLeft auto
                , marginRight auto
                , display block
            ]
        ]
        [ input
            [ value todo.text
            , css
                [ if todo.completed then
                    textDecoration lineThrough

                  else
                    textDecoration none
                , backgroundColor transparent
                , width (vw 60)
                , height (px 50)
                , fontFamilies [ "Courier New" ]
                , fontSize (rem fonts.headingFont)
                , borderRadius (px 5)
                , position (relative)
                , top (px -17)
                ]
            , readonly
                (if todo.editable then
                    False

                 else
                    True
                )
            , onInput (SaveEditTodo index)
            ]
            []
        , button
            [ type_ "button"
            , onClick (EditTodo index)
            , css
                [ fontFamilies [ "Courier New" ]
                , fontSize (rem fonts.baseFont)
                , borderRadius (px 5)
                , backgroundColor transparent
                , border (px 0)
                ]
            ]
            (if todo.editable then
                [ img
                    [ src "./imgs/SaveIcon.svg"
                    , css
                        [ width (px 50)
                        , height (px 50)
                        ]
                    ]
                    []
                ]

             else
                [ img
                    [ src "./imgs/EditIcon.svg"
                    , css
                        [ width (px 50)
                        , height (px 50)
                        ]
                    ]
                    []
                ]
            )
        , button
            [ type_ "button"
            , onClick (ToggleTodo index)
            , css
                [ fontFamilies [ "Courier New" ]
                , fontSize (rem fonts.baseFont)
                , borderRadius (px 5)
                , backgroundColor transparent
                , border (px 0)
                ]
            ]
            (if todo.completed then
                [ img
                    [ src "./imgs/ToggleOn.svg"
                    , css
                        [ width (px 50)
                        , height (px 50)
                        ]
                    ]
                    []
                ]

             else
                [ img
                    [ src "./imgs/ToggleOff.svg"
                    , css
                        [ width (px 50)
                        , height (px 50)
                        ]
                    ]
                    []
                ]
            )
        , button
            [ type_ "button"
            , onClick (RemoveTodo index)
            , css
                [ fontFamilies [ "Courier New" ]
                , fontSize (rem fonts.baseFont)
                , borderRadius (px 5)
                , backgroundColor transparent
                , border (px 0)
                ]
            ]
            [ img
                [ src "./imgs/Delete.svg"
                , css
                    [ width (px 50)
                    , height (px 50)
                    ]
                ]
                []
            ]
        ]


addToList : String -> List Todo -> List Todo
addToList input todos =
    todos ++ [ { text = input, completed = False, editable = False } ]


removeFromList : Int -> List Todo -> List Todo
removeFromList index todos =
    List.take index todos ++ List.drop (index + 1) todos


toggleTodo : Int -> List Todo -> List Todo
toggleTodo index todos =
    List.indexedMap
        (\currentIndex todo ->
            if currentIndex == index then
                { todo | completed = not todo.completed }

            else
                todo
        )
        todos


editTodo : Int -> List Todo -> List Todo
editTodo index todos =
    List.indexedMap
        (\currentIndex todo ->
            if currentIndex == index then
                { todo | editable = not todo.editable }

            else
                todo
        )
        todos


saveEditTodo : Int -> String -> List Todo -> List Todo
saveEditTodo index input todos =
    List.indexedMap
        (\currentIndex todo ->
            if currentIndex == index then
                { todo | text = input }

            else
                todo
        )
        todos


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        AddTodo ->
            { model
                | todos = addToList model.inputText model.todos
                , inputText = ""
            }

        EditTodo index ->
            { model
                | todos = editTodo index model.todos
            }

        SaveEditTodo input index ->
            { model | todos = saveEditTodo input index model.todos }

        RemoveTodo index ->
            { model
                | todos = removeFromList index model.todos
            }

        ToggleTodo index ->
            { model
                | todos = toggleTodo index model.todos
            }

        ChangeInput input ->
            { model | inputText = input }


init : { todos : List a, inputText : String }
init =
    { todos = []
    , inputText = ""
    }


main : Program () Model Msg
main =
    sandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }
