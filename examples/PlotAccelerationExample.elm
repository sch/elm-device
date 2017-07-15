module PlotAccelerationExample exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Lazy exposing (lazy2)
import Svg.Attributes exposing (stroke)
import Task
import Time exposing (Time)
import AnimationFrame
import Window exposing (Size)
import Device.Motion exposing (Motion, Acceleration, initial)
import Plot exposing (defaultSeriesPlotCustomizations)
import SlidingBuffer exposing (SlidingBuffer)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Move Motion
    | Resize Size
    | Tick Time


type alias Model =
    { history : SlidingBuffer Motion
    , dimensions : Size
    , collector : List Motion
    }


init : ( Model, Cmd Msg )
init =
    let
        initialState =
            { history = SlidingBuffer.init 100 Device.Motion.initial
            , dimensions = Size 0 0
            , collector = []
            }
    in
        ( initialState, (Task.perform Resize Window.size) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move motion ->
            ( { model | collector = motion :: model.collector }, Cmd.none )

        Tick _ ->
            ( updateHistory model, Cmd.none )

        Resize dimensions ->
            ( { model | dimensions = dimensions }, Cmd.none )


updateHistory : Model -> Model
updateHistory model =
    let
        newMotion =
            averageAcceleration model.collector
                |> Maybe.withDefault
                    (SlidingBuffer.lastEntered model.history
                        |> Maybe.withDefault Device.Motion.initial
                    )

        history =
            SlidingBuffer.push newMotion model.history
    in
        { model | collector = [], history = history }


averageAcceleration : List Motion -> Maybe Motion
averageAcceleration motions =
    if (List.isEmpty motions) then
        Nothing
    else
        let
            count =
                List.length motions |> toFloat

            acceleration =
                { x = (motions |> List.map (.acceleration >> .x) |> List.sum) / count
                , y = (motions |> List.map (.acceleration >> .y) |> List.sum) / count
                , z = (motions |> List.map (.acceleration >> .z) |> List.sum) / count
                }
        in
            Just { initial | acceleration = acceleration }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Device.Motion.changes Move
        , Window.resizes Resize
        , AnimationFrame.times Tick
        ]


css =
    """
body {
    margin-right: 0;
    font-family: -apple-system, BlinkMacSystemFont, sans-serif;
}

path {
    mix-blend-mode: multiply;
}
"""


view : Model -> Html Msg
view model =
    Html.div []
        [ lazy2 plotMotion model.history model.dimensions
        , Html.node "style" [] [ Html.text css ]
        ]


plotMotion : SlidingBuffer Motion -> Size -> Html Msg
plotMotion history dimensions =
    let
        configuration =
            { defaultSeriesPlotCustomizations
                | margin = { top = 15, bottom = 15, left = 50, right = 0 }
                , width = dimensions.width
                , height = dimensions.height
                , horizontalAxis = Plot.clearAxis
                , toDomainLowest = \y -> y - 10
                , toDomainHighest = \y -> y + 10
            }

        seriesAlong getter color =
            let
                toCoordinates =
                    SlidingBuffer.toList >> (List.indexedMap convertIndexAndMotionToCoordinate)

                convertIndexAndMotionToCoordinate index motion =
                    Plot.clear (toFloat index) (getter motion)
            in
                { axis = Plot.normalAxis
                , interpolation = Plot.Linear (Just color) [ stroke "" ]
                , toDataPoints = toCoordinates
                }

        series =
            [ seriesAlong (.acceleration >> .x) "cyan"
            , seriesAlong (.acceleration >> .y) "magenta"
            , seriesAlong (.acceleration >> .z) "yellow"
            ]
    in
        Plot.viewSeriesCustom configuration series history


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
