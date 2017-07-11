module PlotAccelerationExample exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Time
import Device.Motion exposing (Motion)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Move Motion


type alias Model =
    Motion


init : ( Model, Cmd Msg )
init =
    ( Device.Motion.initial, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move angles ->
            ( angles, Cmd.none )


subscriptions : Motion -> Sub Msg
subscriptions _ =
    Device.Motion.changes Move


view : Model -> Html Msg
view model =
    let
        styles =
            [ ( "padding", "0.618em" )
            , ( "font-family", "-apple-system, BlinkMacSystemFont, sans-serif" )
            , ( "color", "white" )
            , ( "background-color", "rgba(255, 255, 255, 0.1)" )
            , ( "box-shadow", "0 1px 3px rgba(1, 1, 1, 0.1)" )
            , ( "border-radius", "3px" )
            , ( "min-width", "2.2em" )
            , ( "text-align", "right" )
            ]
    in
        center <|
            Html.div
                [ style styles ]
                [ accelerationView "acceleration" model.acceleration
                , accelerationView "including gravity" model.accelerationIncludingGravity
                , rotationView "angular velocity" model.rotationRate
                ]


accelerationView title acceleration =
    Html.div [ style [ ( "margin-bottom", "1em" ) ] ]
        [ Html.div [ style [ ( "text-align", "center" ) ] ] [ Html.text title ]
        , Html.div []
            [ accelerationPartView "x" acceleration.x
            , accelerationPartView "y" acceleration.y
            , accelerationPartView "z" acceleration.z
            ]
        ]


rotationView title velocity =
    Html.div []
        [ Html.text title
        , Html.div []
            [ accelerationPartView "alpha" velocity.alpha
            , accelerationPartView "beta" velocity.beta
            , accelerationPartView "gamma" velocity.gamma
            ]
        ]


accelerationPartView part amount =
    Html.div
        [ style [ ( "display", "flex" ) ] ]
        [ Html.span [] [ Html.text (part ++ ": ") ]
        , Html.span [ style [ ( "flex", "1" ) ] ] [ Html.text (round amount |> toString) ]
        ]


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
