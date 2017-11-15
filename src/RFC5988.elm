module RFC5988 exposing (rfc5988, rfc5988s, Link, emptyLink)

{-| A parser for [the draft for the replacement of RFC5988](https://mnot.github.io/I-D/rfc5988bis/)

@docs Link

# Constructing links
@docs emptyLink

# Parsers
@docs rfc5988, rfc5988s
-}

import Dict exposing (Dict)
import Combine exposing ((<*), (*>), parse, Parser, string, map, succeed, between, regex, sepBy, andThen)


{-| Produce an empty link.
-}
emptyLink : Link
emptyLink =
    { context = ""
    , target = ""
    , relationType = ""
    , targetAttributes = Dict.empty
    }


type alias IRI =
    String


{-| Defined here: https://mnot.github.io/I-D/rfc5988bis/
-}
type alias Link =
    { context : IRI
    , target : IRI
    , relationType : String
    , targetAttributes : Dict String String
    }


{-| Parser for a list of links
-}
rfc5988s : Parser state (List Link)
rfc5988s =
    sepBy (whitespace *> string "," <* whitespace) rfc5988


{-| Parser for a link

      parse rfc5988 "<http://urbit.org>; rel=\"start\"" ==
      ( Ok { context = "", target = "http://urbit.org", relationType = "start", targetAttributes = Dict.empty }
      , { input = "", position = 31 }
      )

-}
rfc5988 : Parser state Link
rfc5988 =
    let
        mergeParameter : ( String, String ) -> Link -> Link
        mergeParameter ( key, value ) link =
            case key of
                "rel" ->
                    { link | relationType = value }

                _ ->
                    { link | targetAttributes = mergeTargetAttribute ( key, value ) link.targetAttributes }

        mergeParameters : List ( String, String ) -> Link -> Link
        mergeParameters params link =
            params
                |> List.foldl mergeParameter link

        mergeTargetAttribute : ( String, String ) -> Dict String String -> Dict String String
        mergeTargetAttribute ( key, value ) acc =
            Dict.insert key value acc

        updateTargetAttributes : Link -> Parser state Link
        updateTargetAttributes link =
            sepBy (string ";") linkParam
                |> map (\keyVals -> mergeParameters keyVals link)
    in
        (carets (regex "[^>]*")
            |> map (\uri -> { emptyLink | target = uri })
        )
            <* string ";"
            |> andThen updateTargetAttributes


linkParam : Parser state ( String, String )
linkParam =
    whitespace
        *> regex "[a-zA-Z]*"
        <* string "="
        |> andThen
            (\key ->
                string "\""
                    *> regex "[a-zA-Z0-9-]*"
                    <* string "\""
                    |> map (\value -> ( key, value ))
            )


whitespace : Parser state String
whitespace =
    regex "[ \t\x0D\n]*"


carets : Parser state res -> Parser state res
carets =
    between (string "<") (string ">")
