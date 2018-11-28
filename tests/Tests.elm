module Tests exposing (all, basicLink, listOfLinks, listOfLinksString)

import Dict exposing (Dict)
import Expect
import Parser exposing (Parser)
import RFC5988
    exposing
        ( Link
        , emptyLink
        , rfc5988
        , rfc5988s
        )
import String
import Test exposing (..)


all : Test
all =
    describe "Parsing RFC5988 Link Headers"
        [ test "Parsing a basic link header value" <|
            \() ->
                Expect.equal (Parser.run rfc5988 "<http://noredink.com>; rel=\"start\"") (Ok basicLink)
        , test "Parsing a link with some additional target attributes" <|
            \() ->
                Expect.equal (Parser.run rfc5988 "<http://noredink.com>; rel=\"start\"; borg=\"unimatrixzero\"") (Ok { basicLink | targetAttributes = Dict.insert "borg" "unimatrixzero" basicLink.targetAttributes })
        , test "Parsing a list of links" <|
            \() ->
                Expect.equal (Parser.run rfc5988s listOfLinksString) (Ok listOfLinks)
        ]


listOfLinksString : String
listOfLinksString =
    "<http://www.example.com/users?page=2>; rel=\"next\", <http://www.example.com/users?page=1>; rel=\"first\", <http://www.example.com/users?page=2>; rel=\"last\""


listOfLinks : List Link
listOfLinks =
    [ { emptyLink | target = "http://www.example.com/users?page=2", relationType = "next" }
    , { emptyLink | target = "http://www.example.com/users?page=1", relationType = "first" }
    , { emptyLink | target = "http://www.example.com/users?page=2", relationType = "last" }
    ]


basicLink : Link
basicLink =
    { context = ""
    , target = "http://noredink.com"
    , relationType = "start"
    , targetAttributes = Dict.empty
    }
