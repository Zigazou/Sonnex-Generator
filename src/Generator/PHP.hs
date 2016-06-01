{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Generator.PHP where

import Data.FileEmbed (embedStringFile)
import Data.List (groupBy)

import Type.Rule

sameStart :: Rule -> Rule -> Bool
sameStart (Rule (Trigger (a:_) _ _) _) (Rule (Trigger (b:_) _ _) _) = a == b
sameStart _ _ = False

phpRules :: [Rule] -> String
phpRules rs = intro ++ (concat . map phpRules') srs ++ outro
    where intro = $(embedStringFile "src/Generator/PHP.intro")
          outro = $(embedStringFile "src/Generator/PHP.outro")
          srs = groupBy sameStart rs

phpRules' :: [Rule] -> String
phpRules' rs = "  if($st1 == \"" ++ first rs ++ "\") {\n"
            ++ (concat . map phpRule) rs
            ++ "  }\n"
    where first ((Rule (Trigger (Letter a:_) _ _) _):_) = [a]
          first _ = []

phpRule :: Rule -> String
phpRule (Rule t a) = "    " ++ phpTrigger t ++ " " ++ phpAction t a ++ "\n"

phpTrigger :: Trigger -> String
phpTrigger (Trigger ls Nothing EndOfWord) =
    "if($st == \"" ++ toString ls ++ "\")"
phpTrigger (Trigger ls Nothing Remains) =
    "if($st" ++ show (length ls) ++ " == \"" ++ toString ls ++ "\")"
phpTrigger (Trigger ls (Just Conson) Remains) =
    "if($st" ++ show (length ls) ++ " == \"" ++ toString ls ++ "\" and \
    \_sonnex_is_conson(mb_substr($st, "++ show (length ls) ++", 1)))"
phpTrigger (Trigger ls (Just Vowel) Remains) =
    "if($st" ++ show (length ls) ++ " == \"" ++ toString ls ++ "\" and \
    \_sonnex_is_vowel(mb_substr($st, "++ show (length ls) ++", 1)))"

stLength :: Trigger -> String
stLength (Trigger ls _ _) = show (length ls)

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
{-
data Recursive = Recursive [Letter] (Maybe PlaceHolder) deriving Show
data Action = Action [Sound] (Maybe Recursive) deriving Show

    if($st4 == "eann") return "an"._sonnex_sonx(mb_substr($st, 4));
    if($st3 == "ean") return '2'._sonnex_sonx(mb_substr($st, 3));
    if($st3 == "eau") return 'o'._sonnex_sonx(mb_substr($st, 3));
    if($st3 == "eff") return "Ef"._sonnex_sonx(mb_substr($st, 3));
    if($st3 == "egm") return 'E'._sonnex_sonx("gm".mb_substr($st, 3));
    if($st == "ein") return "1";
    if($st3 == "ein") {
      $c = mb_substr($st, 3, 1);
      $cs = mb_substr($st, 4);
      if($c == 'n') return "En"._sonnex_sonx($cs);
      if(_sonnex_is_vowel($c)) return "En"._sonnex_sonx($c.$cs);
      return '1'._sonnex_sonx($c.$cs);
    }
    -}