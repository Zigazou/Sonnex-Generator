{-# LANGUAGE FlexibleInstances #-}
module Type.Rule where

class ToString t where
    toString :: t -> String

-- Base types
data PlaceHolder = Conson | Vowel deriving (Eq, Show)
data Letter = Letter Char deriving (Eq, Show)
data Sound = Sound Char deriving (Eq, Show)
data Next = EndOfWord | Remains deriving (Eq, Show)

-- Complex types
data Trigger = Trigger [Letter] (Maybe PlaceHolder) Next deriving Show
data Recursive = Recursive [Letter] (Maybe PlaceHolder) deriving Show
data Action = Action [Sound] (Maybe Recursive) deriving Show
data Rule = Rule Trigger Action deriving Show

-- Instances
instance ToString Letter where
    toString (Letter l) = [l]

instance ToString Sound where
    toString (Sound s) = [s]

instance ToString [Letter] where
    toString ls = concat $ toString <$> ls

instance ToString [Sound] where
    toString ss = concat $ toString <$> ss
