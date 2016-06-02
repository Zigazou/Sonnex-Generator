module Main where

import Text.ParserCombinators.Parsec

import Parser.Parser
import Generator.PHP

main :: IO ()
main = do
    sonnexfile <- readFile "src/sonnex.rules"
    let sonnexrulesE = parse rules "sonnex.rules" sonnexfile

    case sonnexrulesE of
        Left err -> print err
        Right sonnexrules -> putStrLn $ phpRules sonnexrules

