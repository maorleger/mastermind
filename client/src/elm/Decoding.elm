module Decoding exposing (encodeRounds, decodeRounds)

import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=))
import Json.Decode.Extra as Decode exposing ((|:))
import Models exposing (..)
import Http exposing (string)


-- ENCODING


pegToString : Peg -> Encode.Value
pegToString =
    Encode.string << toString


encodeRounds : List Round -> Http.Body
encodeRounds rounds =
    Encode.object
        [ ( "rounds", Encode.list <| List.map encodeRound rounds )
        ]
        |> Encode.encode 0
        |> Http.string


encodeRound : Round -> Encode.Value
encodeRound record =
    let
        toScore score =
            case score of
                Nothing ->
                    [ 0, 0 ]

                Just ( black, white ) ->
                    [ black, white ]
    in
        Encode.object
            [ ( "guess", Encode.list <| List.map pegToString <| record.guess )
            , ( "score", Encode.list <| List.map Encode.int <| toScore record.score )
            ]



-- DECODING


stringToPeg : String -> Decode.Decoder Peg
stringToPeg peg =
    case peg of
        "Orange" ->
            Decode.succeed Orange

        "Yellow" ->
            Decode.succeed Yellow

        "Green" ->
            Decode.succeed Green

        "Red" ->
            Decode.succeed Red

        "Blue" ->
            Decode.succeed Blue

        "Pink" ->
            Decode.succeed Pink

        _ ->
            Decode.fail "that aint a color"


decodeRounds : Decode.Decoder (List Round)
decodeRounds =
    ("rounds" := Decode.list gameRound)


gameRound : Decode.Decoder Round
gameRound =
    Decode.succeed Round
        |: ("guess" := Decode.list decodePeg)
        |: ("score" := (Decode.maybeNull <| Decode.tuple2 (,) Decode.int Decode.int))


decodePeg : Decode.Decoder Peg
decodePeg =
    Decode.andThen Decode.string stringToPeg
