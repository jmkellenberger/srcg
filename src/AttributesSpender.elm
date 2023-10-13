module AttributesSpender exposing (Model, init, update, view)

import Html exposing (Html)
import Html.Events


type Attribute
    = Strength
    | Intelligence
    | Dexterity


attributeToString : Attribute -> String
attributeToString a =
    case a of
        Strength ->
            "Strength"

        Intelligence ->
            "Intelligence"

        Dexterity ->
            "Dexterity"


type alias AttributeValue =
    ( Attribute, Int )


type alias Model =
    List AttributeValue


type Msg
    = Increment Attribute
    | Decrement Attribute


init : Model
init =
    [ ( Strength, 1 )
    , ( Intelligence, 1 )
    , ( Dexterity, 1 )
    ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment a ->
            model

        Decrement a ->
            model


view : Model -> Int -> Html Msg
view model budget =
    Html.div [] <|
        Html.p [] [ Html.text ("Attributes Budget: " ++ String.fromInt budget) ]
            :: List.map viewAttribute model


viewAttribute : ( Attribute, Int ) -> Html Msg
viewAttribute ( attribute, value ) =
    Html.div []
        [ Html.p [] [ Html.text (attributeToString attribute ++ ": " ++ String.fromInt value) ]
        , Html.button [ Html.Events.onClick (Increment attribute) ] [ Html.text "+" ]
        , Html.button [ Html.Events.onClick (Decrement attribute) ] [ Html.text "-" ]
        ]
