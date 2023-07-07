module Kviff.Gps exposing (..)

import Json.Decode
import Parser
import Parser.DeadEnd
import Url


type alias Gps =
    { lat : Float
    , lon : Float
    }


mapyCzLink : Gps -> String
mapyCzLink a =
    "https://mapy.cz/?z=16&y="
        ++ Url.percentEncode (String.fromFloat a.lat)
        ++ "&x="
        ++ Url.percentEncode (String.fromFloat a.lon)
        ++ "&q="
        ++ Url.percentEncode (String.fromFloat a.lat ++ " " ++ String.fromFloat a.lon)


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
