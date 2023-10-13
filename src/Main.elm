module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import List.Extra


type alias Model =
    { priority : List String
    , draggedItem : Maybe String
    }


init : Model
init =
    { priority =
        [ "Attributes"
        , "Magic"
        , "Metatype"
        , "Resources"
        , "Skills"
        ]
    , draggedItem = Nothing
    }


setDraggedItem : String -> Model -> Model
setDraggedItem item model =
    { model | draggedItem = Just item }


clearDraggedItem : Model -> Model
clearDraggedItem model =
    { model | draggedItem = Nothing }


updatePriorities : String -> Model -> Model
updatePriorities target model =
    case model.draggedItem of
        Just dragged ->
            if dragged == target then
                model

            else
                { model
                    | priority = moveItem dragged target model.priority
                }

        Nothing ->
            model


moveItem : String -> String -> List String -> List String
moveItem dragged target list =
    let
        withoutDragged =
            List.filter (\item -> item /= dragged) list

        originalIdx =
            List.Extra.elemIndex dragged list

        newIdx =
            List.Extra.elemIndex target list
    in
    case ( originalIdx, newIdx ) of
        ( Just orig, Just new ) ->
            if orig < new then
                let
                    ( before, after ) =
                        List.Extra.splitAt (new + 1) withoutDragged
                in
                before ++ dragged :: after

            else
                let
                    ( before, after ) =
                        List.Extra.splitAt new withoutDragged
                in
                before ++ dragged :: after

        _ ->
            list


type Msg
    = StartDrag String
    | DragOver String
    | DropOn String


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartDrag item ->
            setDraggedItem item model

        DragOver target ->
            updatePriorities target model

        DropOn target ->
            model
                |> updatePriorities target
                |> clearDraggedItem


view : Model -> Html Msg
view model =
    Html.div [] [ viewPriorties model.priority ]


viewPriorties : List String -> Html Msg
viewPriorties priorities =
    Html.ul [ Attributes.id "priorities" ]
        (List.map viewPriority priorities)


viewPriority : String -> Html Msg
viewPriority p =
    Html.li
        [ Attributes.id p
        , Attributes.draggable "true"
        , Attributes.class "draggable-item"
        , Events.on "dragstart" (Decode.succeed (StartDrag p))
        , Events.preventDefaultOn "drop" (Decode.succeed ( DropOn p, True ))
        , Events.preventDefaultOn "dragover" (Decode.succeed ( DragOver p, True ))
        ]
        [ Html.text p ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
