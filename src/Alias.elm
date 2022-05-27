module Alias exposing (..)

{-| This is the worst code I've ever written in Elm.

I didn't know how I was going to build this until the moment I was vomiting code
into the editor.

But the idea is that an Alias can be described as something that:

1.  Has a matching pattern like "cast \* \*" (which matches "cast fireball skeleton")
2.  Has a replacement pattern like "prepare %1; cast %1 %2; drink mana potion"
3.  Parses user input to match the matching pattern, and then uses it to fill the holes in the
    replacement pattern.

TODO: Clean this up.

-}

import Regex exposing (Regex)
import Set
import String
import Util


regexReplace : String -> (Regex.Match -> String) -> String -> String
regexReplace pattern replacer string =
    case Regex.fromString pattern of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


toRegex : String -> Maybe Regex
toRegex pattern =
    pattern
        |> regexReplace "[ ]+" (\_ -> "\\s+")
        -- |> String.replace "*" "(\\S+)"
        |> String.replace "*" """("[^"]+"|\\S+)"""
        |> (\s -> "^" ++ s ++ "$")
        |> Regex.fromString


validateTemplate : String -> Result Error Int
validateTemplate input_ =
    let
        input =
            String.trim input_
    in
    if String.isEmpty input then
        Err (InvalidValue "cannot be empty")

    else
        let
            regex =
                Regex.fromString "%(\\d+)"
                    |> Maybe.withDefault Regex.never

            strs =
                Regex.find regex input
                    -- "%0 %1 %2 ?"
                    |> List.map .submatches
                    -- [ [Just "0" ], [ Just "1" ], [ Just "2" ], [ Nothing ] ]
                    |> List.concat
                    -- [ Just "0", Just "1", Just "2", Nothing ]
                    |> List.map (Maybe.withDefault "x")

            -- [ "0" "1" "2" "x" ]
        in
        if List.isEmpty strs then
            -- Err (InvalidValue "no %n were found")
            Ok 0

        else if List.any (\s -> s == "x") strs then
            Err (InvalidValue "bad submatch")

        else
            let
                nums : List Int
                nums =
                    strs
                        -- [ "0" "1" "2" ]
                        |> List.map (Maybe.withDefault -1 << String.toInt)
                        -- [0,  1,  2]
                        |> List.sort

                -- [0, 1, 2]
            in
            case some (\n -> n <= 0) nums of
                Just n ->
                    Err (InvalidValue ("expected all wildcards to be >0 but found \"%" ++ String.fromInt n ++ "\""))

                Nothing ->
                    if not (areConsecutive nums) then
                        Err (InvalidValue "wildcards must be consecutive numbers")

                    else if Util.minOfList nums /= Just 1 then
                        let
                            pretty =
                                case Util.minOfList nums of
                                    Nothing ->
                                        "no wildcards"

                                    Just n ->
                                        String.fromInt n
                        in
                        Err (InvalidValue ("expected wildcards to start at %1 but found \"%" ++ pretty ++ "\""))

                    else
                        nums
                            |> Set.fromList
                            |> Set.size
                            |> Ok


some : (a -> Bool) -> List a -> Maybe a
some predicate xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            if predicate x then
                Just x

            else
                some predicate rest


areConsecutive : List Int -> Bool
areConsecutive numbers =
    let
        help : List Int -> Int -> Bool
        help ns prev =
            case ns of
                [] ->
                    True

                next :: rest ->
                    if prev == next || prev + 1 == next then
                        help rest next

                    else
                        False
    in
    case numbers of
        [] ->
            True

        first :: rest ->
            help rest first



{- Create a template from a pattern.

   toTemplate "prepare %1; cast %1 %2;"
       == { string = "prepare %1; cast %1 %2"
          , holeCount = 2
          }

-}
-- toTemplate : String -> Result Error Template
-- toTemplate input =
--     case validateTemplate input of
--         Err msg ->
--             Err msg
--         Ok num ->
--             Ok
--                 { string = input
--                 , holeCount = num
--                 }


{-| A template is the string that has "%1", "%2", etc. holes that
get filled by the input.
-}
type alias Template =
    { string : String
    , holeCount : Int
    }



{- An Alias holds the match pattern "c \* \*" and a template.

   The match pattern is converted into a regex to match user input and parse its holes.
   Then the regex match is used to fill the holes in the template.

       toAlias "c \* \*" "prepare %1; cast %1 %2"
       == { string = "c * *"
          , regex = Regex "^c (.+) (.+)$"
          , template = Template "prepare %1; cast %1 %2"
          }

-}
-- toAlias : String -> String -> Result Error Alias
-- toAlias matcher outPattern =
--     case toTemplate outPattern of
--         Err msg ->
--             Err msg
--         Ok template ->
--             case toRegex matcher of
--                 Nothing ->
--                     Err (InvalidKey "could not be parsed into regex")
--                 Just regex ->
--                     Ok
--                         { string = matcher
--                         , regex = regex
--                         , template = template
--                         }


type alias Alias =
    { -- "c * *"
      matchPattern : String

    -- "^c\\s+(\\S+)\\s+(\\S+)$"
    , regex : Regex

    -- "prepare %1; cast %1 %2"
    , replacePattern : String
    }


trimQuotes : String -> String
trimQuotes input =
    input
        |> String.trim
        |> regexReplace "^\"" (\_ -> "")
        |> regexReplace "\"$" (\_ -> "")


{-| Run an alias against user input to create the final command.

If the user input does not match the alias, then Nothing is returned.

    applyAlias (Alias "c \* \*" "prepare $1; cast %1 %2"" "c sleep gandalf"
    == "prepare sleep; cast sleep gandalf"

TODO: Swap arg order

-}
apply : Alias -> String -> Maybe String
apply alias_ input =
    let
        help n subs acc =
            -- submatchs [ Just "sleep", Just "Gandalf"]
            case subs of
                [] ->
                    Just acc

                Nothing :: _ ->
                    --help (n + 1) rest acc
                    Nothing

                (Just sub) :: rest ->
                    -- sub == "sleep"
                    help (n + 1) rest (String.replace ("%" ++ String.fromInt n) (trimQuotes sub) acc)
    in
    Regex.find alias_.regex (String.trim input)
        |> List.head
        |> Maybe.andThen
            (\match ->
                help 1 match.submatches alias_.replacePattern
            )



-- API
{-

   Alias.parse "c * *" "prepare %1\ncast %1 $2"
       |> Result.map Alias.replacer
       == Ok "prepare %1\ncast %1 %2"

-}
-- replacePattern : Alias -> String
-- replacePattern =
--     .replacePattern
{-

   Alias.parse "c * *" "prepare %1\ncast %1 $2"
       |> Result.map Alias.matcher
       == Ok "c * *"

-}
-- matchPattern : Alias -> String
-- matchPattern =
--     .matchPattern


type Error
    = InvalidKey String
    | InvalidValue String


regexTest : String -> String -> Bool
regexTest pattern input =
    Regex.fromString pattern
        |> Maybe.withDefault Regex.never
        |> (\regex -> Regex.findAtMost 1 regex input)
        |> (\matches -> List.length matches == 1)


validateMatcher : String -> Result Error String
validateMatcher input_ =
    let
        input =
            String.trim input_
    in
    if String.isEmpty input then
        Err (InvalidKey "cannot be empty")

    else if regexTest "\\*{2,}" input then
        Err (InvalidKey "cannot have consecutive \"*\" chars")

    else
        Ok input


countMatcherHoles : String -> Int
countMatcherHoles matchPattern =
    String.split "*" matchPattern
        |> List.length
        |> (\n -> n - 1)


{-|

    Alias.parse "c * *" "prepare %1\ncast %1 $2"
        |> Result.andThen (\al -> Alias.apply "c sleep gandalf")

-}
parse : String -> String -> Result Error Alias
parse matcher_ replacer_ =
    case validateMatcher matcher_ of
        Err error ->
            Err error

        Ok matcher ->
            let
                matcherHoles =
                    countMatcherHoles matcher
            in
            -- toAlias matcher replacer
            case validateTemplate replacer_ of
                Err error ->
                    Err error

                Ok replacerHoles ->
                    if matcherHoles /= replacerHoles then
                        Err
                            (InvalidKey
                                ("matcher has "
                                    ++ String.fromInt matcherHoles
                                    ++ " but replacer has "
                                    ++ String.fromInt replacerHoles
                                    ++ " holes"
                                )
                            )

                    else
                        case toRegex matcher of
                            Nothing ->
                                Err (InvalidKey "could not be parsed into regex")

                            Just regex ->
                                Ok
                                    { matchPattern = matcher
                                    , regex = regex
                                    , replacePattern = String.trim replacer_
                                    }


{-| Returns the first alias in a list that matches the input.
-}
firstMatch : String -> List Alias -> Maybe Alias
firstMatch input aliases =
    Util.some (\a -> apply a input |> Maybe.map (always a)) aliases
