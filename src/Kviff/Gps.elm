module Kviff.Gps exposing (..)

import Json.Decode
import Parser
import Parser.DeadEnd


type alias Gps =
    { lat : Float
    , lon : Float
    }


decode : Json.Decode.Decoder Gps
decode =
    let
        parser : Parser.Parser Gps
        parser =
            Parser.succeed Gps
                |= Parser.float
                |. Parser.symbol ","
                |= Parser.float
                |. Parser.end
    in
    Json.Decode.string
        |> Json.Decode.andThen
            (\x ->
                case Parser.run parser x of
                    Err x2 ->
                        Json.Decode.fail (Parser.DeadEnd.listToString x2)

                    Ok x2 ->
                        Json.Decode.succeed x2
            )
