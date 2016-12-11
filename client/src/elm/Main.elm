module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import String exposing (toLower)
import Http
import Task
import Models exposing (..)
import Decoding exposing (encodeRounds, decodeRounds)


-- APP


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { rounds : List Round, blackPegs : Maybe Int, whitePegs : Maybe Int, gameOver : GameOver }



-- INIT


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model [] Nothing Nothing None
    in
        ( model
        , submitScore model
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


gameOver : List Round -> GameOver
gameOver rounds =
    let
        winRound { guess, score } =
            Maybe.withDefault False (Maybe.map (fst >> ((==) 4)) score)

        loseRound { guess, score } =
            if List.length rounds > 7 || List.isEmpty guess then
                Lose
            else
                None
    in
        case rounds of
            [] ->
                None

            current :: others ->
                if winRound current then
                    Win
                else
                    loseRound current


type Msg
    = NoOp
    | GotRounds (List Round)
    | FailedRounds Http.Error
    | SubmitScore ( Maybe Int, Maybe Int )
    | ChangeBlack String
    | ChangeWhite String
    | NewGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        stringToScore =
            Result.toMaybe << String.toInt

        -- TODO: better error messages if model.blackPegs and model.whitePegs dont exist yet
        -- TODO: refactor all the validation to some applicative
        updateRound { guess, score } =
            let
                orDefault =
                    Maybe.withDefault 0
            in
                Round guess <| Just ( orDefault model.blackPegs, orDefault model.whitePegs )

        validateScore =
            let
                scoreInRange score =
                    if List.member score [ 0, 1, 2, 3, 4 ] then
                        Just score
                    else
                        Nothing
            in
                flip Maybe.andThen scoreInRange
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            GotRounds rounds ->
                ( Model rounds Nothing Nothing (gameOver rounds), Cmd.none )

            FailedRounds error ->
                ( { model | gameOver = Error error }, Cmd.none )

            SubmitScore ( blackPegs, whitePegs ) ->
                let
                    updatedRounds =
                        case model.rounds of
                            [] ->
                                []

                            current :: others ->
                                (updateRound current) :: others

                    newModel =
                        Model updatedRounds Nothing Nothing (gameOver updatedRounds)
                in
                    if newModel.gameOver /= None then
                        ( newModel, Cmd.none )
                    else
                        ( newModel, submitScore newModel )

            ChangeBlack blackPegs ->
                ( { model | blackPegs = validateScore <| stringToScore blackPegs }, Cmd.none )

            ChangeWhite whitePegs ->
                ( { model | whitePegs = validateScore <| stringToScore whitePegs }, Cmd.none )

            NewGame ->
                init


submitScore : Model -> Cmd Msg
submitScore { rounds, blackPegs, whitePegs } =
    let
        server_url =
            "https://play-mastermind.herokuapp.com/play"
    in
        Task.perform FailedRounds GotRounds (Http.post decodeRounds server_url <| encodeRounds rounds)



-- VIEW


view : Model -> Html Msg
view { rounds, blackPegs, whitePegs, gameOver } =
    div [ class "board" ]
        [ (div [ class "rounds" ] <| List.map roundView rounds)
        , (scoreView blackPegs whitePegs gameOver)
        ]


scoreView : Maybe Int -> Maybe Int -> GameOver -> Html Msg
scoreView blackPegs whitePegs gameOver =
    case gameOver of
        Win ->
            gameOverView "Ha! And they say computers are dumb..."

        Lose ->
            gameOverView "Hmmm... I'm stumped. Are you sure you're scoring my guesses accurately?"

        Error error ->
            errorView error

        None ->
            scoreFormView blackPegs whitePegs


errorView : Http.Error -> Html Msg
errorView error =
    let
        errorText =
            case error of
                Http.Timeout ->
                    "Timeout connecting to server..."

                Http.NetworkError ->
                    "Network error connecting to server..."

                Http.UnexpectedPayload payload ->
                    "Hmmm... Got a parse error. Weird..."

                Http.BadResponse code response ->
                    "Hmmm... Got a bad response from the server: " ++ response
    in
        gameOverView errorText


gameOverView : String -> Html Msg
gameOverView msg =
    div [ class "results" ]
        [ div [ class "result__message" ] [ text msg ]
        , div [ class "results__action" ]
            [ button [ class "score__submit--button", onClick NewGame ] [ text "Play Again?" ]
            ]
        ]


scoreFormView : Maybe Int -> Maybe Int -> Html Msg
scoreFormView blackPegs whitePegs =
    let
        parseScore =
            Maybe.map toString >> Maybe.withDefault ""
    in
        div [ class "form" ]
            [ div [ class "score" ]
                [ div [ class "score__input" ]
                    [ label [ for "black-pegs" ] [ text "Black:" ]
                    , input [ class "score__input--black", id "black-pegs", value <| parseScore blackPegs, onInput ChangeBlack ] []
                    ]
                , div [ class "score__input" ]
                    [ label [ for "white-pegs" ] [ text "White:" ]
                    , input [ class "score__input--white", id "white-pegs", value <| parseScore whitePegs, onInput ChangeWhite ] []
                    ]
                ]
            , div [ class "score__submit" ]
                [ button [ class "score__submit--button", onClick <| SubmitScore ( blackPegs, whitePegs ) ] [ text "Score!" ]
                ]
            ]


roundView : Round -> Html Msg
roundView { guess, score } =
    let
        show =
            Maybe.map
                (\( blackPegs, whitePegs ) ->
                    "[" ++ (toString blackPegs) ++ "," ++ (toString whitePegs) ++ "]"
                )
                >> Maybe.withDefault "[ , ]"
    in
        div [ class "round" ]
            [ div [ class "round__pegs" ] <| List.map pegView guess
            , div
                [ class "round__score" ]
                [ text <| show score ]
            ]


pegView : Peg -> Html Msg
pegView peg =
    let
        pegColor =
            "round__peg--" ++ (toLower <| toString peg)
    in
        span [ class <| "round__peg " ++ pegColor ] []
