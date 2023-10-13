module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import List.Extra


type Priority
    = Attributes
    | Magic
    | Metatype
    | Skills
    | Resources


priorityToString : Priority -> String
priorityToString p =
    case p of
        Attributes ->
            "Attributes"

        Magic ->
            "Magic"

        Metatype ->
            "Metatype"

        Skills ->
            "Skills"

        Resources ->
            "Resources"


type alias Model =
    { priority : List Priority
    , draggedItem : Maybe Priority
    }


init : Model
init =
    { priority =
        [ Attributes
        , Magic
        , Metatype
        , Resources
        , Skills
        ]
    , draggedItem = Nothing
    }


priorityToValue : ( Int, Priority ) -> ( Priority, String )
priorityToValue ( idx, priority ) =
    let
        value =
            case priority of
                Metatype ->
                    if idx == 0 then
                        "Metahuman"

                    else
                        "Human"

                Magic ->
                    case idx of
                        0 ->
                            "H Mage"

                        1 ->
                            "H Adept/M Mage"

                        2 ->
                            "M Adept"

                        _ ->
                            "Mundane"

                _ ->
                    String.fromInt idx
    in
    ( priority, value )


setDraggedItem : Priority -> Model -> Model
setDraggedItem item model =
    { model | draggedItem = Just item }


clearDraggedItem : Model -> Model
clearDraggedItem model =
    { model | draggedItem = Nothing }


updatePriorities : Priority -> Model -> Model
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


moveItem : Priority -> Priority -> List Priority -> List Priority
moveItem dragged target list =
    let
        withoutDragged : List Priority
        withoutDragged =
            List.filter (\item -> item /= dragged) list

        originalIdx : Maybe Int
        originalIdx =
            List.Extra.elemIndex dragged list

        newIdx : Maybe Int
        newIdx =
            List.Extra.elemIndex target list

        newIdxFinal o n =
            if o < n then
                n + 1

            else
                n
    in
    Maybe.map2 newIdxFinal originalIdx newIdx
        |> Maybe.map (\newFinalIdx -> insertAt newFinalIdx dragged withoutDragged)
        |> Maybe.withDefault list


insertAt : Int -> a -> List a -> List a
insertAt idx elem list =
    let
        ( before, after ) =
            List.Extra.splitAt idx list
    in
    before ++ elem :: after


type Msg
    = StartDrag Priority
    | DragOver Priority
    | DropOn Priority


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


viewPriorties : List Priority -> Html Msg
viewPriorties priorities =
    Html.ul [ Attributes.id "priorities" ]
        (List.indexedMap
            (\i -> viewPriority << priorityToValue << Tuple.pair i)
            priorities
        )


viewPriority : ( Priority, String ) -> Html Msg
viewPriority ( p, v ) =
    Html.li
        [ Attributes.id (priorityToString p)
        , Attributes.draggable "true"
        , Attributes.class "draggable-item"
        , Events.on "dragstart" (Decode.succeed (StartDrag p))
        , Events.preventDefaultOn "drop" (Decode.succeed ( DropOn p, True ))
        , Events.preventDefaultOn "dragover" (Decode.succeed ( DragOver p, True ))
        ]
        [ Html.text (priorityToString p ++ " - " ++ v) ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
