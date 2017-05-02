module App exposing (..)

import Html exposing (Html, text, div, img)
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Random
import Debug


type alias Model =
    { width : Int
    , height : Int
    , circleMargin : Int
    , currentTime : Time
    , running : Bool
    , startingPosition : Maybe ( Int, Int )
    }


init : ( Model, Cmd Msg )
init =
    ( { width = 600
      , height = 600
      , circleMargin = 50
      , currentTime = 0
      , running = False
      , startingPosition = Nothing
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | Tick Time
    | GenPosition ( Int, Int )
    | Toggle


initPosition : ( Int, Int )
initPosition =
    ( 0, 0 )


randomPoint : Model -> Random.Generator ( Int, Int )
randomPoint model =
    Random.pair
        (Random.int
            model.circleMargin
            (model.width - model.circleMargin)
        )
        (Random.int
            model.circleMargin
            (model.height - model.circleMargin)
        )


randomPosition : Model -> Cmd Msg
randomPosition model =
    Random.generate GenPosition
        (randomPoint model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        GenPosition position ->
            let
                newPosition =
                    if model.running then
                        Just position
                    else
                        Nothing

                newModel =
                    { model | startingPosition = newPosition }
            in
                newModel ! []

        Toggle ->
            { model | running = Basics.not model.running } ! [ randomPosition model ]


view : Model -> Html Msg
view model =
    let
        buttonText =
            case model.running of
                True ->
                    "Stop"

                False ->
                    "Start"
    in
        div []
            [ Html.button [ Html.Events.onClick Toggle ] [ Html.text buttonText ]
            , svg
                [ width (toString <| model.width)
                , height (toString <| model.height)
                , Svg.Attributes.style "background-color: black;"
                ]
                [ singleCircle "red" ( (model.width // 2), model.circleMargin )
                , singleCircle "red" ( model.circleMargin, model.height - model.circleMargin )
                , singleCircle "red" ( model.height - model.circleMargin, model.width - model.circleMargin )
                , startPosition model
                ]
            ]


startPosition : Model -> Svg Msg
startPosition model =
    case model.startingPosition of
        Just ( x, y ) ->
            let
                _ =
                    Debug.log "coords" ( x, y )
            in
                singleCircle "white" ( x, y )

        Nothing ->
            singleCircle "white" ( -50, -50 )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.running of
        True ->
            Time.every Time.millisecond Tick

        False ->
            Sub.none


singleCircle : String -> ( Int, Int ) -> Svg Msg
singleCircle colorText ( xCoord, yCoord ) =
    circle
        [ cx (toString <| xCoord)
        , cy (toString <| yCoord)
        , r "10"
        , fill colorText
        ]
        []
