module Kviff.Api exposing (..)

import Json.Decode as D
import Utils.Json.Decode_ as D_


type alias Item =
    { id : Int
    , type_ : String
    , typeCz : String
    , typeEn : String

    --
    , nameCz : String
    , nameEn : String
    , descriptionCz : String
    , descriptionEn : String

    --
    , timeStart : String
    , timeEnd : String

    --
    , addressId : Int
    , addressNameCz : String
    , addressNameEN : String
    , address : String
    , addressGps : String
    , addressPhone : String
    , addressWebsite : String

    --
    , order : Int
    }



--


decodeItem : D.Decoder Item
decodeItem =
    D.map8
        (\v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 ->
            { timeEnd = v1
            , timeStart = v2
            , id = v3
            , address = v4
            , addressNameCz = v5
            , addressNameEN = v6
            , addressGps = v7
            , addressId = v8
            , addressPhone = v9
            , addressWebsite = v10
            , nameCz = v11
            , nameEn = v12
            , order = v13
            , descriptionCz = v14
            , descriptionEn = v15
            , type_ = v16
            , typeCz = v17
            , typeEn = v18
            }
        )
        (D.field "casDo" D.string)
        (D.field "casOd" D.string)
        (D.field "_id" D.int)
        (D.field "mistoAdresa" D.string)
        (D.field "mistoCz" D.string)
        (D.field "mistoEn" D.string)
        (D.field "mistoGps" D.string)
        (D.field "mistoId" D.int)
        |> D_.apply (D.field "mistoTelefon" D.string)
        |> D_.apply (D.field "mistoWebsite" D.string)
        |> D_.apply (D.field "nazevCz" D.string)
        |> D_.apply (D.field "nazevEn" D.string)
        |> D_.apply (D.field "order" D.int)
        |> D_.apply (D.field "popisCz" D.string)
        |> D_.apply (D.field "popisEn" D.string)
        |> D_.apply (D.field "typ" D.string)
        |> D_.apply (D.field "typCz" D.string)
        |> D_.apply (D.field "typEn" D.string)
