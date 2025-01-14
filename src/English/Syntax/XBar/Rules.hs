module English.Syntax.XBar.Rules where

import Prelude hiding (Word)
import qualified English.Lexicon as L (Word, Feature)
import Universal.Morphosyntax (LexicalCategory(..))


data XCategory = Tense | Complementizer | Inflectional LexicalCategory | Lexical LexicalCategory

data XFeature (c :: XCategory) where
    Imperative    :: XFeature Complementizer
    Interrogative :: XFeature Complementizer
    Declarative   :: XFeature Complementizer
    Grammatical   :: L.Feature lc -> XFeature (Lexical c)

data Leaf (c :: XCategory) where
    Word    :: L.Word lc  -> Leaf (Lexical lc)
    Feature :: XFeature c -> Leaf c
    Null    :: Leaf c

data family XBar (c :: XCategory)
data family XPhrase (c :: XCategory)

-- Inflection
data instance XBar (Inflectional c) =
    I (Leaf (Lexical c))
  | I' (Leaf (Inflectional c)) (XBar (Inflectional c))
  | I'' (XBar (Inflectional c)) (Leaf (Inflectional c))
data instance XPhrase (Inflectional c) = IP (XBar (Inflectional c))

-- Noun
data instance XBar (Lexical Noun) =
    N' (XPhrase (Inflectional Noun))
    | N'' (XPhrase (Lexical Adjective)) (XBar (Lexical Noun))
data instance XPhrase (Lexical Noun) = NP (XBar (Lexical Noun))

-- Determiner
data instance XBar (Lexical Determiner) = D' (Leaf (Lexical Determiner)) (XPhrase (Lexical Noun))
data instance XPhrase (Lexical Determiner) = DP (XBar (Lexical Determiner))

-- Verb
data instance XBar (Lexical Verb) =
    V' (XPhrase (Inflectional Verb)) (XPhrase (Lexical Determiner))
    | V''Adv (XPhrase (Lexical Adverb)) (XBar (Lexical Verb))
    | V''Adv' (XBar (Lexical Verb)) (XPhrase (Lexical Adverb))
    | V''N (XBar (Lexical Verb))  (XPhrase (Lexical Determiner))
data instance XPhrase (Lexical Verb) = VP (XBar (Lexical Verb))

-- Tense
data instance XBar Tense =
    T' (Leaf Tense) (XPhrase (Lexical Verb))
    | T'' (Leaf Tense) (XBar Tense)
    | T''V (Leaf (Lexical Verb)) (XBar Tense)
data instance XPhrase Tense = TP (XPhrase (Lexical Noun)) (XBar Tense)

-- Complementizer
data instance XBar Complementizer = C' (Leaf Tense) (XPhrase Tense)
data instance XPhrase Complementizer =
    CP (Leaf Complementizer) (XBar Complementizer)
    | CP' (Leaf (Lexical SubordinateConjunction)) (XBar Complementizer)

type Sentence = XPhrase Complementizer