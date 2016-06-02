{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Generator.PHP where

import Data.FileEmbed (embedStringFile)
import Data.List (groupBy, partition)

import Type.Rule

sameStart :: Rule -> Rule -> Bool
sameStart (Rule (Trigger _ (a:_) _ _) _) (Rule (Trigger _ (b:_) _ _) _) =
    a == b
sameStart _ _ = False

firstRule :: Rule -> Bool
firstRule (Rule (Trigger (Just StartOfWord) _ _ _) _) = True
firstRule _ = False

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
          srs = groupBy sameStart rs'

phpRules' :: [Rule] -> String
phpRules' rs = "  if($st1 == \"" ++ first rs ++ "\") {\n"
            ++ (concat . map phpRule) rs
            ++ "  }\n"
    where first ((Rule (Trigger _ (Letter a:_) _ _) _):_) = [a]
          first _ = []

phpRule :: Rule -> String
phpRule (Rule t a) = "    " ++ phpTrigger t ++ " " ++ phpAction t a ++ "\n"

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

stLength :: Trigger -> String
stLength (Trigger _ ls _ _) = show (length ls)

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