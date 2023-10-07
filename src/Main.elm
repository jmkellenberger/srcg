module Main exposing (main)

import Browser
import Html exposing (Html)


type alias Model =
    String


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = Noop


init : Model
init =
    "Hello World"


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model


view : Model -> Html Msg
view model =
    Html.div [] [ Html.text model ]
