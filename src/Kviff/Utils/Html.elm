module Kviff.Utils.Html exposing (..)

import Html.Parser
import Parser


stripTags : String -> Result (List Parser.DeadEnd) String
stripTags a =
    let
        nodesToString : List Html.Parser.Node -> String
        nodesToString b =
            List.map nodeToString b |> String.join ""

        nodeToString : Html.Parser.Node -> String
        nodeToString b =
            case b of
                Html.Parser.Text c ->
                    c

                Html.Parser.Element _ _ c ->
                    nodesToString c

                Html.Parser.Comment _ ->
                    ""
    in
    Html.Parser.run a
        |> Result.map nodesToString
