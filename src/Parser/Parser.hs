{- |
Module      : Parser
Description : Parse Sonnex rules
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Parse sonnex rules according to the following grammar:

- endofline = "\n"
- conson = "%"
- vowel = "@"
- remains = "*"
- letter = conson | vowel | "a" | "b" | "c" | ...
- sound = "a" | "b" | "E" | ...
- endofword = "."
- transform = "->"
- output = ":"
- line = space* . (rule . space*)? . comment? . endofline
- comment = "#" . char* . endofline
- rule = trigger . space* . transform . space* . action
- trigger = endofword? . letter+ . (endofword | remains)
- action = sound* . (output . letter* . remains)?

-}
module Parser.Parser (rules) where
import Text.ParserCombinators.Parsec ( GenParser, oneOf, optionMaybe, many1
                                     , string, many, eof, noneOf
                                     )
import Text.Parsec.Char (char, endOfLine)
import Data.Maybe (catMaybes)
import Control.Monad (liftM)

import Type.Rule ( Rule(Rule)
                 , Next(EndOfWord, Remains)
                 , Recursive(Recursive)
                 , Letter(Letter)
                 , Action(Action, acSounds, acRecursive)
                 , Sound(Sound)
                 , Start(StartOfWord)
                 , PlaceHolder(Conson, Vowel)
                 , Trigger(Trigger, trStart, trLetters, trEnd, trPlaceHolder)
                 )

{-| List of characters accepted as a `Letter`
-}
letterChars :: String
letterChars = ['a'..'z'] ++ "àâäéèêëîïôöùûüç'’œ"

{-| List of characters accepted as a `Sound`
-}
soundChars :: String
soundChars = "abCdeEfgijklmnoprstuUvz123"

{-| Parse a list of `Rule`
-}
rules :: GenParser Char st [Rule]
rules = liftM catMaybes (many statement) <* eof

{-| Parse a comment
    A comment starts with '#' and contains character till the end of line
-}
comment :: GenParser Char st String
comment = char '#' *> many (noneOf "\r\n")

{-| Parse a `Statement`
    A `Statement` is a line which may contains a `Rule` and/or a comment.
-}
statement :: GenParser Char st (Maybe Rule)
statement = rspaces *> optionMaybe rule <* optionMaybe comment <* endOfLine

{-| Parse many spaces
    The spaces parser is not used because it parses end of line like a space
    but the language is line-oriented.
-}
rspaces :: GenParser Char st String
rspaces = many (oneOf " \t")

{-| Parse a `Rule`
    A `Rule` is made of a `Trigger` and an `Action` separated by the '->'
    string.
-}
rule :: GenParser Char st Rule
rule = do
    t <- trigger
    _ <- rspaces
    _ <- string "->"
    _ <- rspaces
    a <- action
    _ <- rspaces
    return $ Rule t a

{-| Parse an `EndOfWord` or a `Remains`
-}
next :: GenParser Char st Next
next = do
    ch <- oneOf ".*"
    case ch of
        '.' -> return EndOfWord
        _ -> return Remains

{-| Parse a `StartOfWord`
-}
start :: GenParser Char st Start
start = char '.' >> return StartOfWord

{-| Parse a `Trigger`
-}
trigger :: GenParser Char st Trigger
trigger = do
    st <- optionMaybe start
    letters <- many1 (oneOf letterChars)
    ph <- optionMaybe placeholder
    end <- next
    return Trigger { trStart = st
                   , trLetters = Letter <$> letters
                   , trPlaceHolder = ph
                   , trEnd = end
                   }

{-| Parse a `PlaceHolder`
-}
placeholder :: GenParser Char st PlaceHolder
placeholder = do
    ch <- oneOf "%@"
    case ch of
        '%' -> return Conson
        _ -> return Vowel

{-| Parse an `Action`
-}
action :: GenParser Char st Action
action = do
    sounds <- many (oneOf soundChars)

    isRecursive <- optionMaybe (char ':')
    recursive <- case isRecursive of
        Just _ -> do
            letters <- many (oneOf letterChars)
            ph <- optionMaybe placeholder
            _ <- char '*'
            return $ Just $ Recursive (Letter <$> letters) ph
        Nothing -> return Nothing

    return Action { acSounds = Sound <$> sounds
                  , acRecursive = recursive
                  }