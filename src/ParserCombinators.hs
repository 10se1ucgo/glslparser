{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

-- CIS 552, University of Pennsylvania
-- Slight modifications from myself (Hammaad)

-- | A small, applicative parsing library
module ParserCombinators
  ( GenParser,
    Parser,
    ParseError,
    doParse,
    get,
    eof,
    filter,
    parse,
    parseFromFile,
    satisfy,
    alpha,
    digit,
    upper,
    lower,
    space,
    char,
    string,
    int,
    chainl1,
    chainl,
    choice,
    between,
    sepBy1,
    sepBy,
    parseSeq,
    parseSeq2
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Data.Char
import System.IO
import Prelude hiding (filter)

-- definition of the parser type
-- Hammaad: I have modified this to work on not just strings, sorta...
newtype GenParser v a = P {doParse :: [v] -> Maybe (a, [v])}
-- Parses [Char] == strings into an a
type Parser = GenParser Char

instance Functor (GenParser v) where
  fmap :: (a -> b) -> GenParser v a -> GenParser v b
  fmap f p = P $ \s -> do
    (c, cs) <- doParse p s
    return (f c, cs)

instance Applicative (GenParser v) where
  pure x = P $ \s -> Just (x, s)
  p1 <*> p2 = P $ \s -> do
    (f, s') <- doParse p1 s
    (x, s'') <- doParse p2 s'
    return (f x, s'')

instance Alternative (GenParser v) where
  empty = P $ const Nothing
  p1 <|> p2 = P $ \s -> doParse p1 s `firstJust` doParse p2 s

-- | Return the next character from the input
get :: GenParser v v
get = P $ \s -> case s of
  (c : cs) -> Just (c, cs)
  [] -> Nothing

-- | This parser *only* succeeds at the end of the input.
eof :: GenParser v ()
eof = P $ \s -> case s of
  [] -> Just ((), [])
  _ : _ -> Nothing

-- | Filter the parsing results by a predicate
filter :: (v -> Bool) -> GenParser v v -> GenParser v v
filter f p = P $ \s -> do
  (c, cs) <- doParse p s
  guard (f c)
  return (c, cs)

-- | Combine two Maybe values together, producing the first
-- successful result
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing y = y

---------------------------------------------------------------
---------------------------------------------------------------
---------------------------------------------------------------

type ParseError = String

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors, but we
-- give it a type similar to other Parsing libraries.
parse :: GenParser v a -> [v] -> Either ParseError a
parse parser str = case doParse parser str of
  Nothing -> Left "No parses"
  Just (a, _) -> Right a

-- | parseFromFile p filePath runs a string parser p on the input
-- read from filePath using readFile. Returns either a
-- ParseError (Left) or a value of type a (Right).
parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do
  handle <- openFile filename ReadMode
  str <- hGetContents handle
  pure $ parse parser str

-- | Return the next character if it satisfies the given predicate
satisfy :: (v -> Bool) -> GenParser v v
satisfy p = filter p get

-- | Parsers for specific sorts of characters
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace

-- | Parses and returns the specified character
-- succeeds only if the input is exactly that character
char :: Eq v => v -> GenParser v v
char c = satisfy (c ==)

-- | Parses and returns the specified string.
-- Succeeds only if the input is the given string
string :: String -> Parser String
string = foldr (\c p -> (:) <$> char c <*> p) (pure "")

-- | succeed only if the input is a (positive or negative) integer
int :: Parser Int
int = read <$> (((++) <$> string "-" <*> some digit) <|> some digit)

-- | Parses one or more occurrences of @p@ separated by bindary operator
-- parser @pop@.  Returns a value produced by a /left/ associative application
-- of all functions returned by @pop@.
-- See the end of the Parsers lecture for explanation of this operator.
chainl1 :: GenParser v a -> GenParser v (a -> a -> a) -> GenParser v a
p `chainl1` pop = foldl comb <$> p <*> rest
  where
    comb x (op, y) = x `op` y
    rest = many ((,) <$> pop <*> p)

-- | @chainl p pop x@ parses zero or more occurrences of @p@, separated by @pop@.
-- If there are no occurrences of @p@, then @x@ is returned.
chainl :: Parser b -> Parser (b -> b -> b) -> b -> Parser b
chainl p pop x = chainl1 p pop <|> pure x

-- | Combine all parsers in the list (sequentially)
choice :: [GenParser v a] -> GenParser v a
choice = foldr (<|>) empty

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is pureed.
between :: GenParser v open -> GenParser v a -> GenParser v close -> GenParser v a
between open p close = open *> p <* close

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: GenParser v a -> GenParser v sep -> GenParser v [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: GenParser v a -> GenParser v sep -> GenParser v [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- Hammaad:
-- I don't know what to call this
-- Sorta similar to `string`?
parseSeq :: [GenParser v a] -> (a -> a -> a) -> GenParser v a
parseSeq []     f = empty
parseSeq [p]    f = p
parseSeq (p:ps) f = P $ \s -> do
    (a, s') <- doParse p s
    (b, s'') <- doParse (parseSeq ps f) s'
    return (a `f` b, s'')

parseSeq2 :: [GenParser v a] -> (a -> b -> b) -> b -> GenParser v b
parseSeq2 []     f i = empty
parseSeq2 [p]    f i = (\x -> f x i) <$> p
parseSeq2 (p:ps) f i = P $ \s -> do
    (a, s') <- doParse p s
    (b, s'') <- doParse (parseSeq2 ps f i) s'
    return (a `f` b, s'')