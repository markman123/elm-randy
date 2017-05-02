module App exposing (..)

import Html exposing (Html, text, div, img)
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Random
import Math.Vector2 exposing (Vec2, getX, getY)


type alias Model =
    { width : Int
    , height : Int
    , circleMargin : Int
    , currentTime : Time
    , running : Bool
    , startingPosition : Maybe Vec2
    , currentPosition : Maybe Vec2
    , addCircles : List Vec2
    }


init : ( Model, Cmd Msg )
init =
    ( { width = 600
      , height = 600
      , circleMargin = 50
      , currentTime = 0
      , running = False
      , startingPosition = Nothing
      , currentPosition = Nothing
      , addCircles = []
      }
    , Cmd.none
    )


type Msg
    = Tick Time
    | GenPosition ( Float, Float )
    | Toggle
    | GenNewPoint Int
    | Step


randomPoint : Model -> Random.Generator ( Float, Float )
randomPoint model =
    Random.pair
        (Random.float
            (model.circleMargin |> toFloat)
            ((model.width - model.circleMargin) |> toFloat)
        )
        (Random.float
            (model.circleMargin |> toFloat)
            ((model.height - model.circleMargin) |> toFloat)
        )


randomPosition : Model -> Cmd Msg
randomPosition model =
    Random.generate GenPosition
        (randomPoint model)


rollDice : Cmd Msg
rollDice =
    Random.generate GenNewPoint (Random.int 1 3)


getCoords : Model -> Int -> Vec2
getCoords model n =
    case n of
        1 ->
            ( (model.width // 2), model.circleMargin ) |> toVec

        2 ->
            ( model.circleMargin, model.height - model.circleMargin ) |> toVec

        3 ->
            ( model.height - model.circleMargin, model.width - model.circleMargin ) |> toVec

        _ ->
            ( 0, 0 ) |> toVec


toVec : ( Int, Int ) -> Vec2
toVec ( x, y ) =
    Math.Vector2.vec2 (toFloat x) (toFloat y)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            { model | currentTime = newTime } ! [ rollDice ]

        Step ->
            { model | running = True } ! [ randomPosition model, rollDice ]

        GenPosition ( newX, newY ) ->
            let
                newVec =
                    Math.Vector2.vec2 newX newY

                newPosition =
                    if model.running then
                        Just newVec
                    else
                        Nothing

                newModel =
                    { model | startingPosition = newPosition, currentPosition = newPosition }
            in
                newModel ! []

        Toggle ->
            let
                newModel =
                    if model.running then
                        { model | running = False, addCircles = [] }
                    else
                        { model | running = True }
            in
                newModel ! [ randomPosition model ]

        GenNewPoint newPoint ->
            let
                _ =
                    Debug.log "rolled" newPoint

                newModel =
                    plotNewPoint newPoint model
            in
                newModel ! []


plotNewPoint : Int -> Model -> Model
plotNewPoint newPoint model =
    let
        fromVec =
            getCoords model newPoint

        toVec =
            model.currentPosition |> Maybe.withDefault (Math.Vector2.vec2 0.0 0.0)

        diff =
            Math.Vector2.distance toVec fromVec

        newVec =
            calcNewPoint toVec diff
    in
        { model | addCircles = [ newVec ] ++ model.addCircles }


calcNewPoint : Vec2 -> Float -> Vec2
calcNewPoint from diff =
    let
        newX =
            (Math.Vector2.getX from) + (diff / 2)

        newY =
            (Math.Vector2.getY from) + (diff / 2)

        newVec =
            Math.Vector2.vec2 newX newY
    in
        newVec


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
            , Html.button [ Html.Events.onClick Step ] [ Html.text "Step" ]
            , svg
                [ width (toString <| model.width)
                , height (toString <| model.height)
                , Svg.Attributes.style "background-color: black;"
                ]
                ([ singleCircle "red" ( (model.width // 2), model.circleMargin )
                 , singleCircle "red" ( model.circleMargin, model.height - model.circleMargin )
                 , singleCircle "red" ( model.height - model.circleMargin, model.width - model.circleMargin )
                 , startPosition model
                 ]
                    ++ additionalCircles model
                )
            ]


additionalCircles : Model -> List (Svg Msg)
additionalCircles model =
    List.map additionalCircle model.addCircles


additionalCircle : Vec2 -> Svg Msg
additionalCircle coord =
    singleCircle "white"
        ( (Math.Vector2.getX coord) |> round, (Math.Vector2.getY coord) |> round )


startPosition : Model -> Svg Msg
startPosition model =
    case model.startingPosition of
        Just coord ->
            let
                x =
                    round <| (getX coord)

                y =
                    round <| (getY coord)
            in
                singleCircle "yellow" ( x, y )

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
singleCircle colorText ( x, y ) =
    circle
        [ cx (toString <| x)
        , cy (toString <| y)
        , r "10"
        , fill colorText
        ]
        []
