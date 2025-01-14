module English.Lexicon (Word, Feature) where

import Prelude hiding (Word)
import Data.Map (Map, empty)
import Universal.Morphosyntax (LexicalCategory(..))
import English.Grammar (Feature(..))

type Word c = String

type Inflection c = [Feature c] -> String

data Definition c = Definition {
    root     :: Word c,
    forms    :: Inflection c,
    features :: [Feature c]
}

type D = Word Determiner
type N = Word Noun
type V = Word Verb
type Adj = Word Adjective
type Adv = Word Adverb

-----------------------
-- reserved keywords --
-----------------------
keyVerbs :: Map String (Definition Verb)
keyVerbs = empty