{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Module      : PHP
Description : Generate a Sonnex function based on a list of `Rule`
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Generator.PHP (phpRules) where

import Data.FileEmbed (embedStringFile)
import Data.List (groupBy, partition, elemIndex, sortOn)

import Type.Rule ( Rule(Rule)
                 , Trigger(Trigger)
                 , Action(Action)
                 , Recursive(Recursive)
                 , Next(EndOfWord, Remains)
                 , Letter(Letter)
                 , Start(StartOfWord)
                 , PlaceHolder(Conson, Vowel)
                 , toString
                 )

-- | Letters ordered by the frequency in the french language
frequentLetters :: String
frequentLetters = "esaitnrulodcpmévqfbghjàxyèêzwçùkîœïëôöû'’â"

-- | Give a weight to a group of `Rule` according to letters frequency
groupWeight :: [Rule] -> Int
groupWeight ((Rule (Trigger _ (Letter a:_) _ _) _):_) =
    case elemIndex a frequentLetters of
         Just i -> i
         Nothing -> 1000
groupWeight _ = 1000

-- | Give a weight to a subgroup of `Rule` according to letters frequency
groupWeight2 :: [Rule] -> Int
groupWeight2 ((Rule (Trigger _ (_:Letter a:_) _ _) _):_) =
    case elemIndex a frequentLetters of
         Just i -> i
         Nothing -> 900
groupWeight2 ((Rule (Trigger _ [_] _ EndOfWord) _):_) = (-1)
groupWeight2 ((Rule (Trigger _ [_] _ Remains) _):_) = 1000
groupWeight2 _ = 950

-- | Checks if two `Rule`’s `Trigger` start with the same letter.
sameStart :: Rule -> Rule -> Bool
sameStart (Rule (Trigger _ (a:_) _ _) _) (Rule (Trigger _ (b:_) _ _) _) =
    a == b
sameStart _ _ = False

-- | Checks if two `Rule`’s `Trigger` start with the same second letter.
sameStart2 :: Rule -> Rule -> Bool
sameStart2 (Rule (Trigger _ (_:a:_) _ _) _) (Rule (Trigger _ (_:b:_) _ _) _) =
    a == b
sameStart2 _ _ = False

-- | Checks if the `Rule` only works at the start of a word
firstRule :: Rule -> Bool
firstRule (Rule (Trigger (Just StartOfWord) _ _ _) _) = True
firstRule _ = False

-- | Compile a list of `Rule` into PHP code
phpRules :: [Rule] -> String
phpRules rs = concat
    [ "<?php\n"
    , utils
    , "function _sonnex_sonx0($st) {\n"
    , intro0
    , (concat . map phpRule) frs
    , outro0
    , "}\n\n"
    , "function _sonnex_sonx($st) {\n"
    , intro
    , (concat . map phpRules') srs
    , outro
    , "}\n\n"
    , sonnx
    ]
    where utils  = $(embedStringFile "src/Generator/PHP.utils")
          sonnx  = $(embedStringFile "src/Generator/PHP.sonnex")
          intro  = $(embedStringFile "src/Generator/PHP.intro")
          outro  = $(embedStringFile "src/Generator/PHP.outro")
          intro0 = $(embedStringFile "src/Generator/PHP.0.intro")
          outro0 = $(embedStringFile "src/Generator/PHP.0.outro")
          (frs, rs') = partition firstRule rs
          {- 2 optimisations:
             - tests are grouped by their first letter
             - tests are ordered on letters frequency
           -}
          srs = sortOn groupWeight $ groupBy sameStart rs'

-- | Compile a group of `Rule` into PHP code
phpRules' :: [Rule] -> String
phpRules' rs = "  if($st1 == \"" ++ first rs ++ "\") {\n"
            ++ ( concat
               . map optimize
               . sortOn groupWeight2
               . groupBy sameStart2
               ) rs
            ++ "  }\n"
    where first ((Rule (Trigger _ (Letter a:_) _ _) _):_) = [a]
          first _ = []
          second ((Rule (Trigger _ (_:Letter a:_) _ _) _):_) = [a]
          second _ = []
          -- Optimize subgroups
          optimize [r] = phpRule r
          optimize [r1, r2] = phpRule r1 ++ phpRule r2
          optimize rs' = "  if($sc2 == \"" ++ second rs' ++ "\") {\n"
                      ++ (concat . map phpRule) rs'
                      ++ "  }\n"

-- | Compile a single `Rule` into PHP code
phpRule :: Rule -> String
phpRule (Rule t a) = "    " ++ phpTrigger t ++ " " ++ phpAction t a ++ "\n"

-- | Compile a `Trigger` into PHP code
phpTrigger :: Trigger -> String
phpTrigger (Trigger _ ls Nothing EndOfWord) =
    "if($st == \"" ++ toString ls ++ "\")"
phpTrigger (Trigger _ ls Nothing Remains) =
    "if($st" ++ show (length ls) ++ " == \"" ++ toString ls ++ "\")"
phpTrigger (Trigger _ ls (Just Conson) Remains) =
    "if($st" ++ show (length ls) ++ " == \"" ++ toString ls ++ "\" and \
    \_sonnex_is_conson(mb_substr($st, "++ show (length ls) ++", 1)))"
phpTrigger (Trigger _ ls (Just Vowel) Remains) =
    "if($st" ++ show (length ls) ++ " == \"" ++ toString ls ++ "\" and \
    \_sonnex_is_vowel(mb_substr($st, "++ show (length ls) ++", 1)))"
phpTrigger (Trigger _ ls (Just Conson) EndOfWord) =
    "if($length == " ++ show (length ls + 1) ++ " and \
    \$st" ++ show (length ls) ++ " == \"" ++ toString ls ++ "\" and \
    \_sonnex_is_conson(mb_substr($st, "++ show (length ls) ++", 1)))"
phpTrigger (Trigger _ ls (Just Vowel) EndOfWord) =
    "if($length == " ++ show (length ls + 1) ++ " and \
    \$st" ++ show (length ls) ++ " == \"" ++ toString ls ++ "\" and \
    \_sonnex_is_vowel(mb_substr($st, "++ show (length ls) ++", 1)))"

-- | Calculate the length of the constant part of a `Trigger`
stLength :: Trigger -> String
stLength (Trigger _ ls _ _) = show (length ls)

-- | Compile an `Action` given its `Trigger` into PHP code
phpAction :: Trigger -> Action -> String
phpAction _ (Action ss Nothing) = concat
    [ "return '"
    , toString ss
    , "';"
    ]
phpAction t (Action ss (Just (Recursive [] Nothing))) = concat
    [ "return \""
    , toString ss
    , "\"._sonnex_sonx(mb_substr($st, "
    , stLength t
    , "));"
    ]
phpAction t (Action ss (Just (Recursive ls Nothing))) = concat
    [ "return \""
    , toString ss
    , "\"._sonnex_sonx(\""
    , toString ls
    , "\".mb_substr($st, "
    , stLength t
    , "));"
    ]
phpAction t (Action ss (Just (Recursive [] (Just _)))) = concat
    [ "return \""
    , toString ss
    , "\"._sonnex_sonx(mb_substr($st, "
    , stLength t
    , "));"
    ]
phpAction t (Action ss (Just (Recursive ls (Just _)))) = concat
    [ "return \""
    , toString ss
    , "\"._sonnex_sonx(\""
    , toString ls
    , "\".mb_substr($st, "
    , stLength t
    , "));"
    ]