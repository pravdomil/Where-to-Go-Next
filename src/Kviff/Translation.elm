module Kviff.Translation exposing (..)

import Kviff.Api as Api


title : String
title =
    "KVIFF Program"


footer : String
footer =
    "You are welcome!"


type_ : Api.Type -> String
type_ a =
    case a of
        Api.Event_ ->
            "Event"

        Api.Daily ->
            "Daily"

        Api.Talk ->
            "Talk"

        Api.Exhibition ->
            "Exhibition"

        Api.Restaurant ->
            "Restaurant"
