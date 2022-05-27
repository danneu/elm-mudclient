module AliasTests exposing (..)

import Alias
import Bitwise exposing (or)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Regex
import Test exposing (..)


alias1 =
    Alias.parse "c * *" "prepare %1; cast %1 %2"
        |> Result.withDefault (Alias.Alias "nope" Regex.never "nope")


templateValidationTests =
    describe "template validation"
        [ test "%0 is illegal" <|
            \_ ->
                Expect.err (Alias.validateTemplate "%0")
        ]


consecutiveTests =
    describe "areConsecutive"
        [ test "empty list is consecutive" <|
            \_ ->
                Expect.equal True (Alias.areConsecutive [])
        , test "equal numbers are consecutive" <|
            \_ ->
                Expect.equal True (Alias.areConsecutive [ 1, 1, 1 ])
        , test "incremental numbers are consecutive" <|
            \_ ->
                Expect.equal True (Alias.areConsecutive [ 1, 2, 3 ])
        , test "decremental numbers are not consecutive" <|
            \_ ->
                Expect.equal False (Alias.areConsecutive [ 3, 2, 1 ])
        ]


holesTests =
    describe "countHoles"
        [ test "works1" <|
            \_ ->
                Expect.err (Alias.validateTemplate "")
        , test "works2" <|
            \_ ->
                Expect.equal (Ok 3) (Alias.validateTemplate "%1 %2 %3")
        , test "works3" <|
            \_ ->
                Expect.equal (Ok 1) (Alias.validateTemplate "%1 %1 %1")
        , test "works4" <|
            \_ ->
                Expect.err (Alias.validateTemplate "%1231")
        , test "works5" <|
            \_ ->
                Expect.err (Alias.validateTemplate "%2")
        ]


basicTests =
    describe "basic"
        [ test "works" <|
            \_ ->
                Expect.equal (Just "prepare sleep; cast sleep gandalf") (Alias.apply alias1 "c sleep gandalf")
        , test "too-many-args" <|
            \_ ->
                Expect.equal Nothing (Alias.apply alias1 "c sleep gandalf third")
        , test "too-few-args" <|
            \_ ->
                Expect.equal Nothing (Alias.apply alias1 "c sleep ")
        , test "no-args" <|
            \_ ->
                Expect.equal Nothing (Alias.apply alias1 "c ")
        , test "whitespace" <|
            \_ ->
                Expect.equal (Just "prepare sleep; cast sleep gandalf") (Alias.apply alias1 "  c   sleep    gandalf    ")
        ]


quoteTests =
    let
        al =
            Alias.parse "a * *" "a (%1) (%2)"
                |> Result.withDefault (Alias.Alias "x" Regex.never "x")
    in
    describe "quotes"
        [ test "a b c" <|
            \_ ->
                Expect.equal (Just "a (b) (c)") (Alias.apply al "a b c")
        , test "a \"b c\" d" <|
            \_ ->
                Expect.equal (Just "a (b c) (d)") (Alias.apply al "a \"b c\" d")
        , test "a b \"c d\"" <|
            \_ ->
                Expect.equal (Just "a (b) (c d)") (Alias.apply al "a b \"c d\"")
        , test "a \"b c\" \"d e\"" <|
            \_ ->
                Expect.equal (Just "a (b c) (d e)") (Alias.apply al "a \"b c\" \"d e\"")
        , test "bad-quotes1" <|
            \_ ->
                Expect.equal Nothing (Alias.apply al "a \"b c\" \"d e")
        , test "bad-quotes2" <|
            \_ ->
                Expect.equal Nothing (Alias.apply al "a \"b c\" d e\"")
        ]


dummyAlias : Alias.Alias
dummyAlias =
    Alias.Alias "invalid" Regex.never "invalid"


alias2Tests =
    let
        match =
            "gq *"

        tpl =
            "get %1 bag; quaff %1"

        alias_ =
            Alias.parse match tpl
                |> Result.withDefault dummyAlias
    in
    describe "gq *"
        [ test "1" <|
            \_ ->
                Expect.ok (Alias.parse match tpl)

        -- , test "2" <|
        --     \_ ->
        --         Expect.ok (Alias.toTemplate tpl)
        , test "works" <|
            \_ ->
                Expect.equal (Just "get potion bag; quaff potion") (Alias.apply alias_ "gq potion")
        ]
