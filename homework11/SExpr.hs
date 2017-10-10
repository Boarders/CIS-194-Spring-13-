{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace


-- parses an identifier which is an alphabetic character followed by zero or more alphanumeric characters. 
ident :: Parser String
ident = (:) <$> satisfy (isAlpha) <*> (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

-- parseSExp "(bar (foo))" = Comb [ A (I "bar"), Comb [A (I "foo")]]
-- parseSExp "(foo)" = Comb [A I ("foo")]

bracketSExpr ::  SExpr -> SExpr
bracketSExpr (A at) = Comb [A at]
bracketSExpr (Comb sexp) = Comb ([Comb sexp])

parseSExpr :: Parser SExpr
parseSExpr = spaces *> ( parseAtom <|> parseBracketExp ) <* spaces
  where parseBracketExp = Comb <$> (char '(' *> oneOrMore parseSExpr <* char ')')

parseAtom :: Parser SExpr
parseAtom = (parseAtomInt <|> parseAtomIdent)



parseAtomInt :: Parser SExpr
parseAtomInt = (.) A N <$> posInt

parseAtomIdent :: Parser SExpr
parseAtomIdent = (.) A I <$> ident
