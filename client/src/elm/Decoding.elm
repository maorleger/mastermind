module Decoding exposing (encodeRounds, decodeRounds)

import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Extra as Decode exposing ((|:))
import Models exposing (..)
import Http


-- ENCODING


pegToString : Peg -> Encode.Value
pegToString =
    Encode.string << toString


encodeRounds : List Round -> Http.Body
encodeRounds rounds =
    Http.jsonBody <|
        Encode.object
            [ ( "rounds", Encode.list <| List.map encodeRound rounds )
            ]


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
        Encode.object <|
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
    (Decode.field "rounds" <| Decode.list gameRound)


gameRound : Decode.Decoder Round
gameRound =
    Decode.succeed Round
        |: (Decode.field "guess" <| Decode.list decodePeg)
        |: (Decode.field "score" <| (Decode.nullable <| Decode.map2 (,) (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)))


decodePeg : Decode.Decoder Peg
decodePeg =
    Decode.andThen stringToPeg <| Decode.string
