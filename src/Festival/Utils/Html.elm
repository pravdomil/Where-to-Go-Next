module Festival.Utils.Html exposing (..)

import Html.Parser


stripTags : String -> String
stripTags a =
    let
        nodesToString : List Html.Parser.Node -> String
        nodesToString b =
            String.join "" (List.map nodeToString b)

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
        |> Result.withDefault a
