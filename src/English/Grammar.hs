module English.Grammar (Feature, Tense, Class) where

import Universal.Morphosyntax (LexicalCategory(..))

type Tense = Present | Past
type Class = Number
type Case  = String

data Feature (c :: LexicalCategory) where
    Case  :: Case  -> Feature a
    Tense :: Tense -> Feature Verb
    Class :: Class -> Feature Noun
