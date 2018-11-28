module RFC5988 exposing
    ( Link
    , emptyLink
    , rfc5988, rfc5988s
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
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


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
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = whitespace
        , item = rfc5988
        , trailing = Parser.Forbidden
        }


{-| Parser for a link

      parse rfc5988 "<http://noredink.com>; rel=\"start\"" ==
      ( Ok { context = "", target = "http://noredink.com", relationType = "start", targetAttributes = Dict.empty }
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
            Parser.sequence
                { start = ""
                , separator = ";"
                , end = ""
                , spaces = Parser.chompWhile (\_ -> False)
                , item = linkParam
                , trailing = Parser.Forbidden
                }
                |> Parser.map (\keyVals -> mergeParameters keyVals link)
    in
    Parser.andThen updateTargetAttributes
        (Parser.succeed (\uri -> { emptyLink | target = uri })
            |= carets
            |. Parser.symbol ";"
        )


linkParam : Parser.Parser ( String, String )
linkParam =
    Parser.succeed Tuple.pair
        |. whitespace
        |= (any [ isBetween 'a' 'z', isBetween 'A' 'Z' ]
                |> Parser.chompWhile
                |> Parser.getChompedString
           )
        |. Parser.symbol "="
        |. Parser.symbol "\""
        |= (any [ isBetween 'a' 'z', isBetween 'A' 'Z', isBetween '0' '9', (==) '-' ]
                |> Parser.chompWhile
                |> Parser.getChompedString
           )
        |. Parser.symbol "\""


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
    Parser.succeed identity
        |. Parser.symbol "<"
        |= Parser.getChompedString (Parser.chompUntil ">")
        |. Parser.symbol ">"


whitespace : Parser ()
whitespace =
    Parser.chompWhile (\c -> Set.member c whitespaceChars)
        |> Parser.getChompedString
        |> Parser.map (\_ -> ())


whitespaceChars : Set Char
whitespaceChars =
    Set.fromList [ ' ', '\t', '\u{000D}', '\n' ]
