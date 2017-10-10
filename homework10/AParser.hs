{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

instance Functor Parser where
  fmap f (Parser parse) = Parser (g.parse)
    where g = fmap $ first f

instance Applicative Parser where
  pure a = Parser (\st -> Just (a, st))
  -- (<*>) :: Parse (a->b) -> Parse a -> Parse b
  --     === (String -> Maybe (a->b, String)) -> Parse a -> Parse b
  (Parser parse) <*> (Parser  parse') = Parser parse''
    where parse'' st =
            case parse st of
              Nothing -> Nothing
              Just (f, st') -> (case (parse' st') of
                                  Nothing -> Nothing
                                  Just (a, st'') -> Just (f a, st''))

parse'a, parse'b, parse'space :: Parser Char
parse'a = char 'a'
parse'b = char 'b'
parse'space = char ' '

pair :: a -> b -> (a,b)
pair a b = (a,b)

abParser :: Parser (Char,Char)
abParser = pair <$> parse'a <*> parse'b

discardParse :: Parser a -> Parser ()
discardParse = fmap (const ()) 


abParser_ :: Parser ()
abParser_ = discardParse abParser

pairIntegers :: Integer -> Char -> Integer -> [Integer]
pairIntegers n c m = [n,m]

intPair :: Parser [Integer]
intPair = pairIntegers <$> posInt <*> parse'space <*> posInt

instance Alternative Parser where
  empty = Parser (\st->Nothing)
  parser1 <|> parser2 = Parser (\st -> (runParser parser1 st) <|> (runParser parser2 st))

charNegate :: Char -> Integer -> Integer
charNegate c n = negate n

negInt :: Parser Integer
negInt = charNegate <$> char '-' <*> posInt

parseInt :: Parser Integer
parseInt = posInt <|> negInt

intOrUppercase :: Parser ()
intOrUppercase = discardint <|> discardUpper
  where discardint = discardParse parseInt
        discardUpper = discardParse $ satisfy isUpper

