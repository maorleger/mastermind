module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onClick, onInput)
import String exposing (toLower)
import Http
import Models exposing (..)
import Decoding exposing (encodeRounds, decodeRounds)


-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { showIntro: Bool
    , rounds : List Round
    , blackPegs : Int
    , whitePegs : Int
    , gameOver : GameOver
    , thinking : Bool
    , threeDMode : Bool
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model True [] 0 0 None False False

        submitScoreCmd =
            Http.send SubmitScoreResponse (submitScore model)
    in
        ( model
        , submitScoreCmd
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
            Maybe.withDefault False (Maybe.map (Tuple.first >> ((==) 4)) score)

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
    | Begin
    | GotRounds (List Round)
    | FailedRounds Http.Error
    | SubmitScoreResponse (Result Http.Error (List Round))
    | SubmitScore
    | ChangeBlack String
    | ChangeWhite String
    | IncBlack
    | IncWhite
    | NewGame
    | Toggle3D


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateRound { guess, score } =
            Round guess <| Just ( model.blackPegs, model.whitePegs )

        parseScore score currentTargetScore currentOtherScore =
            let
                validate score =
                    if List.member score [ 0, 1, 2, 3, 4 ] && score + currentOtherScore <= 4 then
                        Just score
                    else
                        Nothing
            in
                String.toInt score
                    |> Result.toMaybe
                    |> Maybe.andThen validate
                    |> Maybe.withDefault currentTargetScore
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            Begin -> 
                ( { model | showIntro = False }, Cmd.none )

            GotRounds rounds ->
                ( { model | rounds = rounds, gameOver = gameOver rounds }, Cmd.none )

            FailedRounds error ->
                ( { model | gameOver = Error error }, Cmd.none )

            SubmitScoreResponse (Err error) ->
                Debug.log (toString error)
                    ( { model | gameOver = Error error }, Cmd.none )

            SubmitScoreResponse (Ok rounds) ->
                ( { model | rounds = rounds, gameOver = gameOver rounds, thinking = False }, Cmd.none )

            SubmitScore ->
                let
                    updatedRounds =
                        case model.rounds of
                            [] ->
                                []

                            current :: others ->
                                (updateRound current) :: others

                    newModel =
                        { model
                        | blackPegs = 0
                        , whitePegs = 0
                        , rounds = updatedRounds
                        , gameOver = gameOver updatedRounds
                        , thinking = True
                        }
                in
                    if newModel.gameOver /= None then
                        ( newModel, Cmd.none )
                    else
                        let
                            submitScoreCmd =
                                Http.send SubmitScoreResponse <| submitScore newModel
                        in
                            ( newModel, submitScoreCmd )

            ChangeBlack blackPegs ->
                ( { model | blackPegs = parseScore blackPegs model.blackPegs model.whitePegs }, Cmd.none )

            ChangeWhite whitePegs ->
                ( { model | whitePegs = parseScore whitePegs model.whitePegs model.blackPegs }, Cmd.none )

            IncBlack ->
                ( { model | blackPegs = parseScore ( toString <| ( model.blackPegs + 1 ) % 5 ) model.blackPegs model.whitePegs }, Cmd.none )

            IncWhite ->
                ( { model | whitePegs = parseScore ( toString <| ( model.whitePegs + 1 ) % 5 ) model.whitePegs model.blackPegs }, Cmd.none )

            NewGame ->
                init

            Toggle3D ->
                ( { model | threeDMode = not model.threeDMode }, Cmd.none )


submitScore : Model -> Http.Request (List Round)
submitScore { rounds, blackPegs, whitePegs } =
    let
        server_url =
            "https://play-mastermind.herokuapp.com/play"

        --"http://localhost:3000/play"
    in
        Http.post
            server_url
            (encodeRounds rounds)
            decodeRounds



-- VIEW


view : Model -> Html Msg
view { showIntro, rounds, blackPegs, whitePegs, gameOver, threeDMode, thinking } =
    let
        board =
            div [ class "game" ]
                [ div [ classList [ ("board", True), ("board--3d", threeDMode) ] ]
                    [ div [ class "rounds" ] <| List.map roundView rounds
                    , div [ class "score-note" ] [ scoreView thinking blackPegs whitePegs gameOver ]
                    , span [ class "three-d", onClick Toggle3D ] [ text <| if threeDMode then "2D" else "3D" ]
                    ]
                ]

        intro = 
            div [ class "intro" ]
                [ div [ class "intro__pegs" ]
                    [ span [ class <| "round__peg round__peg--blue" ] []
                    , span [ class <| "round__peg round__peg--pink" ] []
                    , span [ class <| "round__peg round__peg--yellow" ] []
                    , span [ class <| "round__peg round__peg--orange" ] []
                    , span [ class <| "round__peg round__peg--green" ] []
                    , span [ class <| "round__peg round__peg--red" ] []
                    ]
                , h3 [] [ text "Let's play!" ]
                , p [] [ text "You pick a secret code.  It can be any combination of 4 of the colors above.  Remember it, but don't tell me!" ]
                , p [] [ text "Ready to play?" ]
                , button [ class "score__submit", onClick Begin ] [ text "Begin" ]
                ]

    in
        if showIntro then intro else board


scoreView : Bool -> Int -> Int -> GameOver -> Html Msg
scoreView thinking blackPegs whitePegs gameOver =
    case gameOver of
        Win ->
            gameOverView "Ha! And they say computers are dumb..." True

        Lose ->
            gameOverView "Hmmm... I'm stumped. Are you sure you're scoring my guesses accurately?" True

        Error error ->
            errorView error

        None ->
            if thinking then
                gameOverView "Hmm... Let me think..." False
            else
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

                Http.BadUrl _ ->
                    "Seems like you entered an invalid url for the server"

                Http.BadStatus _ ->
                    "Got a bad status code from the server..."

                Http.BadPayload _ _ ->
                    "Got something back from the server, but it's not something I can parse"
    in
        gameOverView errorText True


gameOverView : String -> Bool -> Html Msg
gameOverView msg showButton =
    div [ class "results" ]
        <| h3 [ class "result__message" ] [ text msg ]
        :: if showButton then
            [ div [ class "results__action" ] [ button [ class "score__submit", onClick NewGame ] [ text "Play Again?" ] ] ]
                else
            []


scoreFormView : Int -> Int -> Html Msg
scoreFormView blackPegs whitePegs =
    Html.form [ class "form", onSubmit <| SubmitScore ]
        [ h3 [] [text "Here's my guess.  How did I do?"]
        , div [ class "score" ]
            [ div [ class "score__input" ]
                [ label [ for "black-pegs"
                        , class "score__peg score__peg--black"
                        , onClick IncBlack
                        ] []
                , span [ class "score__value" ] [ text <| toString blackPegs ]
                , input [ class "score__input--black"
                        , id "black-pegs"
                        , Html.Attributes.max "4"
                        , Html.Attributes.min "0"
                        , type_ "number"
                        , autofocus True
                        , value <| toString blackPegs
                        , onInput ChangeBlack
                        ] []
                ]
            , div [ class "score__input" ]
                [ label [ for "white-pegs"
                        , class "score__peg score__peg--white"
                        , onClick IncWhite
                        ] []
                , span [ class "score__value" ] [ text <| toString whitePegs ]
                , input [ class "score__input--white"
                        ,id "white-pegs"
                        , Html.Attributes.max "4"
                        , Html.Attributes.min "0"
                        , type_ "number"
                        , value <| toString whitePegs
                        , onInput ChangeWhite
                        ] []
                ]
            ]
        , button [ class "score__submit", type_ "submit" ] [ text "Score!" ]
        , p [] [text "(Black pegs = number of right color, right place)" ]
        , p [] [text "(White pegs = number of right color, wrong place)" ]
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
            , div [ class "round__score" ]
                <| Maybe.withDefault []
                <| Maybe.map
                (\(black, white) ->
                    [ div [ class "score__peg score__peg--black" ] [ text <| toString <| black ]
                    , div [ class "score__peg score__peg--white" ] [ text <| toString <| white ]
                    ]
                )
                <| score
            ]


pegView : Peg -> Html Msg
pegView peg =
    let
        pegColor =
            "round__peg--" ++ (toLower <| toString peg)
    in
        span [ class <| "round__peg " ++ pegColor ] []
