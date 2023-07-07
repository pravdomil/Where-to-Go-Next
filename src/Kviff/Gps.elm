module Kviff.Gps exposing (..)

import Json.Decode
import Parser
import Parser.DeadEnd


type alias Gps =
    { lat : Float
    , lon : Float
    }


decoder : Json.Decode.Decoder Gps
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\x ->
                case Parser.run parser x of
                    Err x2 ->
                        Json.Decode.fail (Parser.DeadEnd.listToString x2)

                    Ok x2 ->
                        Json.Decode.succeed x2
            )


parser : Parser.Parser Gps
parser =
    Parser.float
        |> Parser.andThen
            (\x ->
                Parser.symbol ","
                    |> Parser.map (\() -> x)
            )
        |> Parser.andThen
            (\x ->
                Parser.float
                    |> Parser.map (\x2 -> Gps x x2)
            )
        |> Parser.andThen
            (\x ->
                Parser.end
                    |> Parser.map (\() -> x)
            )
