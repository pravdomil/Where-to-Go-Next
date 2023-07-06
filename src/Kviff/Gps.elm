module Kviff.Gps exposing (..)

import Json.Decode as D
import Parser as P exposing ((|.), (|=))
import Parser.DeadEnd as DeadEnd


type alias Gps =
    { lat : Float
    , lon : Float
    }


decode : D.Decoder Gps
decode =
    let
        parser : P.Parser Gps
        parser =
            P.succeed Gps
                |= P.float
                |. P.symbol ","
                |= P.float
                |. P.end
    in
    D.string
        |> D.andThen
            (\v ->
                case P.run parser v of
                    Err b ->
                        D.fail (DeadEnd.toString b)

                    Ok b ->
                        D.succeed b
            )
