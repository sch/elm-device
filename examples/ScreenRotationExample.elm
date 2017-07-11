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
    ( Device.Orientation.initial, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reorient angles ->
            ( angles, Cmd.none )


subscriptions : EulerRotation -> Sub Msg
subscriptions _ =
    Device.Orientation.changes Reorient


view : Model -> Html Msg
view model =
    let
        screenRotation =
            toString (floor model.alpha) ++ "°"

        rotateZ =
            "rotateZ(" ++ toString model.alpha ++ "deg)"

        rotateX =
            "rotateX(" ++ toString ((clamp -90 90 model.beta) / 3) ++ "deg)"

        rotateY =
            "rotateY(" ++ toString (negate model.gamma / 3) ++ "deg)"

        styles =
            [ ( "padding", "0.618em" )
            , ( "font-family", "-apple-system, BlinkMacSystemFont, sans-serif" )
            , ( "font-size", "200%" )
            , ( "font-weight", "500" )
            , ( "color", "white" )
            , ( "transform", "perspective(500px) " ++ rotateX ++ " " ++ rotateY ++ " " ++ rotateZ )
            , ( "background-color", "rgba(255, 255, 255, 0.1)" )
            , ( "box-shadow", "0 1px 3px rgba(1, 1, 1, 0.1)" )
            , ( "border-radius", "3px" )
            , ( "min-width", "2.2em" )
            , ( "text-align", "right" )
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
