module Parser.Parser where
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.Maybe (catMaybes)
import Control.Monad (liftM)

import Type.Rule

vowelChars :: String
vowelChars = "aâàäeéèêëiîïoôöuùûüyœ"

consonChars :: String
consonChars = "bcçdfghjklmnpqrstvwxyz"

letterChars :: String
letterChars = ['a'..'z'] ++ "àâäéèêëîïôöùûüç'’œ"

soundChars :: String
soundChars = "abCdeEfgijklmnoprstuUvz123"

rules :: GenParser Char st [Rule]
rules = liftM catMaybes (many statement) <* eof

comment :: GenParser Char st String
comment = char '#' *> many (noneOf "\r\n")

statement :: GenParser Char st (Maybe Rule)
statement = rspaces *> optionMaybe rule <* optionMaybe comment <* endOfLine

rspaces :: GenParser Char st String
rspaces = many (oneOf " \t")

rule :: GenParser Char st Rule
rule = do
    t <- trigger
    _ <- rspaces
    _ <- string "->"
    _ <- rspaces
    a <- action
    _ <- rspaces
    return $ Rule t a

next :: GenParser Char st Next
next = do
    ch <- oneOf ".*"
    case ch of
        '.' -> return EndOfWord
        _ -> return Remains

trigger :: GenParser Char st Trigger
trigger = do
    letters <- many1 (oneOf letterChars)
    ph <- optionMaybe placeholder
    end <- next
    return $ Trigger (Letter <$> letters) ph end

placeholder :: GenParser Char st PlaceHolder
placeholder = do
    ch <- oneOf "%@"
    case ch of
        '%' -> return Conson
        _ -> return Vowel

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

    return $ Action (Sound <$> sounds) recursive

