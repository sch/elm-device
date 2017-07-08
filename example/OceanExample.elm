module OceanExample exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Orientation exposing (Orientation, EulerRotation)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Reorient Orientation


type alias Model =
    EulerRotation


init : ( Model, Cmd Msg )
init =
    ( Orientation.default, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reorient angles ->
            ( angles, Cmd.none )


subscriptions : Orientation -> Sub Msg
subscriptions model =
    Orientation.changes (\thing -> Reorient thing)


view : Model -> Html Msg
view model =
    let
        rotation =
            toString model.alpha ++ " deg"

        styles =
            [ ( "padding", "2em" )
            , ( "font-family", "-apple-system, BlinkMacSystemFont, sans-serif" )
            ]
    in
        Html.div [ style styles ] [ Html.text rotation ] |> center


center : Html Msg -> Html Msg
center node =
    let
        styles =
            [ ( "position", "absolute" )
            , ( "top", "0" )
            , ( "bottom", "0" )
            , ( "left", "0" )
            , ( "right", "0" )
            , ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            ]
    in
        Html.div [ style styles ] [ node ]
