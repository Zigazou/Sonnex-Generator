{-# LANGUAGE FlexibleInstances #-}
{- |
Module      : Rule
Description : Types used to represent Sonnex’s rules
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Type.Rule
( Rule(Rule, ruTrigger, ruAction)
, Next(EndOfWord, Remains)
, Recursive(Recursive, reLetters, rePlaceHolder)
, Letter(Letter)
, Action(Action, acSounds, acRecursive)
, Sound(Sound)
, Start(StartOfWord)
, PlaceHolder(Conson, Vowel)
, Trigger(Trigger, trStart, trLetters, trEnd, trPlaceHolder)
, toString
)
where

class ToString t where
    toString :: t -> String

-- | A `PlaceHolder` may represent either a `Conson` or a `Vowel`
data PlaceHolder = Conson | Vowel deriving (Eq, Show)

-- | A `Letter` represents a letter found in french language
data Letter = Letter Char deriving (Eq, Show)

-- | A `Sound` represents a letter which always is associated with a sound
data Sound = Sound Char deriving (Eq, Show)

-- | A `Next` allows to specify a `Trigger` which ends or continues
data Next = EndOfWord | Remains deriving (Eq, Show)

-- | A `Start` is used to indicate that a `Trigger` must start a word
data Start = StartOfWord deriving (Eq, Show)

-- | A `Trigger` is a pattern which will trigger an `Action`
data Trigger = Trigger
    { trStart :: Maybe Start
    , trLetters :: [Letter]
    , trPlaceHolder :: Maybe PlaceHolder
    , trEnd :: Next
    }  deriving Show

-- | A `Recursive` is part of an `Action` which will run the automaton again
data Recursive = Recursive
    { reLetters :: [Letter]
    , rePlaceHolder :: Maybe PlaceHolder
    } deriving Show

-- | An `Action` is made of `Sound`
data Action = Action
    { acSounds :: [Sound]
    , acRecursive :: Maybe Recursive
    } deriving Show

-- | A `Rule` is a couple `Trigger`/`Action`
data Rule = Rule
    { ruTrigger :: Trigger
    , ruAction :: Action
    } deriving Show

-- Instances
instance ToString Letter where
    toString (Letter l) = [l]

instance ToString Sound where
    toString (Sound s) = [s]

instance ToString [Letter] where
    toString ls = concat $ toString <$> ls

instance ToString [Sound] where
    toString ss = concat $ toString <$> ss
