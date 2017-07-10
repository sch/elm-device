module ScreenRotationExample exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Device.Orientation exposing (EulerRotation)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Reorient EulerRotation


type alias Model =
    EulerRotation


init : ( Model, Cmd Msg )
init =
    ( Orientation.initial, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reorient angles ->
            ( angles, Cmd.none )


subscriptions : EulerRotation -> Sub Msg
subscriptions _ =
    Orientation.changes Reorient


view : Model -> Html Msg
view model =
    let
        screenRotation =
            toString (floor model.alpha) ++ " deg"

        styles =
            [ ( "padding", "2em" )
            , ( "font-family", "-apple-system, BlinkMacSystemFont, sans-serif" )
            , ( "font-size", "300%" )
            , ( "font-weight", "500" )
            , ( "color", "white" )
            , ( "transform", "rotate(" ++ toString model.alpha ++ "deg)" )
            ]
    in
        Html.div [ style styles ] [ Html.text screenRotation ] |> center


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
            , ( "background-color", "#60B5CC" )
            ]
    in
        Html.div [ style styles ] [ node ]
