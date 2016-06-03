{- |
Module      : Sonnex-Generator
Description : Takes a rules file and compiles it into PHP code
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Main where

import Text.ParserCombinators.Parsec (parse)

import Parser.Parser (rules)
import Generator.PHP (phpRules)

-- | Sonnex-Generator
main :: IO ()
main = do
    -- Load the rules file
    sonnexfile <- readFile "src/sonnex.rules"

    -- Parses every rule
    let sonnexrulesE = parse rules "sonnex.rules" sonnexfile

    -- Compiles the rules into PHP code
    case sonnexrulesE of
        Left err -> print err
        Right sonnexrules -> putStrLn $ phpRules sonnexrules
