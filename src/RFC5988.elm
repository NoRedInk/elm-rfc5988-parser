module RFC5988
    exposing
        ( Link
        , emptyLink
        , rfc5988
        , rfc5988s
        )

{-| A parser for [the draft for the replacement of RFC5988](https://mnot.github.io/I-D/rfc5988bis/)

@docs Link


# Constructing links

@docs emptyLink


# Parsers

@docs rfc5988, rfc5988s

-}

import Char
import Dict exposing (Dict)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , ignore
        , keep
        , succeed
        , symbol
        , zeroOrMore
        )
import Parser.LanguageKit


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


{-| Defined here: <https://mnot.github.io/I-D/rfc5988bis/>
-}
type alias Link =
    { context : IRI
    , target : IRI
    , relationType : String
    , targetAttributes : Dict String String
    }


{-| Parser for a list of links
-}
rfc5988s : Parser (List Link)
rfc5988s =
    Parser.LanguageKit.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = whitespace
        , item = rfc5988
        , trailing = Parser.LanguageKit.Forbidden
        }


{-| Parser for a link

      parse rfc5988 "<http://urbit.org>; rel=\"start\"" ==
      ( Ok { context = "", target = "http://urbit.org", relationType = "start", targetAttributes = Dict.empty }
      , { input = "", position = 31 }
      )

-}
rfc5988 : Parser Link
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

        updateTargetAttributes : Link -> Parser Link
        updateTargetAttributes link =
            Parser.LanguageKit.sequence
                { start = ""
                , separator = ";"
                , end = ""
                , spaces =
                    ignore Parser.zeroOrMore (always False)
                , item = linkParam
                , trailing = Parser.LanguageKit.Forbidden
                }
                |> Parser.map (\keyVals -> mergeParameters keyVals link)
    in
    Parser.andThen updateTargetAttributes
        (succeed (\uri -> { emptyLink | target = uri })
            |= carets
            |. symbol ";"
        )


linkParam : Parser ( String, String )
linkParam =
    succeed (,)
        |. whitespace
        |= keep zeroOrMore
            (any
                [ isBetween 'a' 'z'
                , isBetween 'A' 'Z'
                ]
            )
        |. symbol "="
        |. symbol "\""
        |= keep zeroOrMore
            (any
                [ isBetween 'a' 'z'
                , isBetween 'A' 'Z'
                , isBetween '0' '9'
                , (==) '-'
                ]
            )
        |. symbol "\""


any : List (a -> Bool) -> a -> Bool
any fs x =
    case fs of
        [] ->
            False

        f :: rest ->
            if f x then
                f x
            else
                any rest x


isBetween : Char -> Char -> Char -> Bool
isBetween low high char =
    let
        code =
            Char.toCode char
    in
    (code >= Char.toCode low) && (code <= Char.toCode high)


carets : Parser String
carets =
    succeed identity
        |. symbol "<"
        |= keep zeroOrMore ((/=) '>')
        |. symbol ">"


whitespace : Parser ()
whitespace =
    ignore zeroOrMore
        (\char ->
            List.any ((==) char)
                [ ' ', '\t', '\x0D', '\n' ]
        )
