module Universal.Morphosyntax (LexicalCategory(..)) where

data LexicalCategory = Determiner | Noun | Pronoun | Verb | Adjective | Adverb | Adposition
    | Conjunction | Interjection | Particle | SubordinateConjunction

data family Feature c