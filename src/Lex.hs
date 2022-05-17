{-# LANGUAGE InstanceSigs #-}
-- The GLSL language is specified in terms of tokens, this module will
-- read in an input program as a string/file and spit out a list of tokens
module Lex (Keyword (..), Operator (..), Token (..), tokenize, tokenizeOrDie, tokenizeFromFile, tokenizeFromFileOrDie, TParser)
    where

import ParserCombinators

import Control.Monad
import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Functor
import Test.QuickCheck
import qualified Data.List as List

data Keyword = KW_attribute | KW_const | KW_uniform | KW_varying | KW_centroid
             | KW_break | KW_continue | KW_do | KW_for | KW_while | KW_if | KW_else
             | KW_in | KW_out | KW_inout
             | KW_float | KW_int | KW_void | KW_bool
             | KW_true | KW_false | KW_invariant | KW_discard | KW_return
             | KW_mat2 | KW_mat3 | KW_mat4
             | KW_vec2 | KW_vec3 | KW_vec4
             | KW_ivec2 | KW_ivec3 | KW_ivec4
             | KW_bvec2 | KW_bvec3 | KW_bvec4
             | KW_sampler1D | KW_sampler2D | KW_sampler3D | KW_samplerCube
    deriving (Show, Eq, Bounded, Enum)

instance Arbitrary Keyword where
    arbitrary :: Gen Keyword
    arbitrary = arbitraryBoundedEnum

-- May god have mercy on my soul
-- If a string is a keyword, return a KW token.
readKW :: String -> Maybe Keyword
readKW "samplerCube"= Just KW_samplerCube
readKW "attribute"  = Just KW_attribute
readKW "invariant"  = Just KW_invariant
readKW "sampler1D"  = Just KW_sampler1D
readKW "sampler2D"  = Just KW_sampler2D
readKW "sampler3D"  = Just KW_sampler3D
readKW "centroid"   = Just KW_centroid
readKW "continue"   = Just KW_continue
readKW "uniform"    = Just KW_uniform
readKW "varying"    = Just KW_varying
readKW "discard"    = Just KW_discard
readKW "return"     = Just KW_return
readKW "const"      = Just KW_const
readKW "break"      = Just KW_break
readKW "while"      = Just KW_while
readKW "inout"      = Just KW_inout
readKW "float"      = Just KW_float
readKW "false"      = Just KW_false
readKW "ivec2"      = Just KW_ivec2
readKW "ivec3"      = Just KW_ivec3
readKW "ivec4"      = Just KW_ivec4
readKW "bvec2"      = Just KW_bvec2
readKW "bvec3"      = Just KW_bvec3
readKW "bvec4"      = Just KW_bvec4
readKW "else"       = Just KW_else
readKW "void"       = Just KW_void
readKW "bool"       = Just KW_bool
readKW "true"       = Just KW_true
readKW "mat2"       = Just KW_mat2
readKW "mat3"       = Just KW_mat3
readKW "mat4"       = Just KW_mat4
readKW "vec2"       = Just KW_vec2
readKW "vec3"       = Just KW_vec3
readKW "vec4"       = Just KW_vec4
readKW "for"        = Just KW_for
readKW "out"        = Just KW_out
readKW "int"        = Just KW_int
readKW "do"         = Just KW_do
readKW "if"         = Just KW_if
readKW "in"         = Just KW_in
readKW _            = Nothing

-- Reads a token either as a keyword if it matches one or a regular identifier
readIdentKW :: String -> Token
readIdentKW i = maybe (Identifier i) KW (readKW i)

-- nondigit = _ | a-z | A-Z
isNonDigit :: Char -> Bool
isNonDigit c = isAsciiLower c || isAsciiUpper c || c == '_'

-- ident = nondigit | ident nondigit | ident digit
parseIdent :: Parser String
parseIdent = (:) <$> satisfy isNonDigit <*> many (satisfy isNonDigit <|> digit)

-- Generate a random identifier, making sure it begins with a non-digit.
genIdent :: Gen String
genIdent = (:) <$> elements nondigit <*> listOf (elements (nondigit ++ digit)) where
    nondigit  = ['A'..'Z'] ++ ['a'..'z']
    digit     = ['0'..'9']

-- Read a floating point literal
parseFloat :: Parser Float
parseFloat = read <$> parseFloat' where
    parseFloat' :: Parser String
    parseFloat' = (parseSeq [parseFract, parseExp <|> string ""] (++)
               <|> parseSeq [some digit, parseExp] (++)) <* parseOptSuffix

    -- Optionally parse a 'f' floating point suffix
    parseOptSuffix :: Parser String
    parseOptSuffix = string "f" <|> string "F" <|> string ""

    -- Parse an exponent part, (e|E)<sign>?<digit sequence>
    parseExp :: Parser String
    parseExp = parseSeq [string "e" <|> string "E", parseOptSign, some digit] (++)

    -- Optionally parse a sign (+|-)?
    parseOptSign :: Parser String
    parseOptSign = string "+" <|> string "-" <|> string ""

    parseFract :: Parser String
    parseFract = choice [parseSeq [some digit, string ".", some digit] (++),
                         -- We force a "0" at the end of this input since read :: Float does not like trailing decimal points
                         parseSeq [some digit, string ".", string "" $> "0"] (++),
                         -- Same for leading.
                         parseSeq [ string "" $> "0", string ".", some digit] (++)]

parseIdentKW :: Parser Token
parseIdentKW = readIdentKW <$> parseIdent


-- The order here matters (a bit), we want to match the longest string we can.
-- So, longer operators need to be before the shorter ones.
-- [O_PlusPlus .. O_ModAssign] are 2 char operators
-- The rest are 1 char operators
data Operator = O_PlusPlus | O_MinusMinus | O_LShift | O_RShift
              | O_LE | O_GE | O_EQ | O_NE | O_LAnd | O_LXor | O_LOr
              | O_PlusAssign | O_MinusAssign | O_MultAssign | O_DivAssign | O_ModAssign
              | O_LParen | O_RParen | O_LBracket | O_RBracket | O_Dot
              | O_Plus | O_Minus | O_Tilde | O_Not | O_Mult | O_Div | O_Mod
              | O_LT | O_GT | O_And | O_Xor | O_Or
              | O_QMark | O_Colon | O_Assign | O_Comma
    deriving (Eq, Bounded, Enum)

instance Show Operator where
   show O_PlusPlus    = "++"
   show O_MinusMinus  = "--"
   show O_LShift      = "<<"
   show O_RShift      = ">>"
   show O_LE          = "<="
   show O_GE          = ">="
   show O_EQ          = "=="
   show O_NE          = "!="
   show O_LAnd        = "&&"
   show O_LXor        = "^^"
   show O_LOr         = "||"
   show O_PlusAssign  = "+="
   show O_MinusAssign = "-="
   show O_MultAssign  = "*="
   show O_DivAssign   = "/="
   show O_ModAssign   = "%="
   show O_LParen      = "("
   show O_RParen      = ")"
   show O_LBracket    = "["
   show O_RBracket    = "]"
   show O_Dot         = "."
   show O_Plus        = "+"
   show O_Minus       = "-"
   show O_Tilde       = "~"
   show O_Not         = "!"
   show O_Mult        = "*"
   show O_Div         = "/"
   show O_Mod         = "%"
   show O_LT          = "<"
   show O_GT          = ">"
   show O_And         = "&"
   show O_Xor         = "^"
   show O_Or          = "|"
   show O_QMark       = "?"
   show O_Colon       = ":"
   show O_Assign      = "="
   show O_Comma       = ","

instance Arbitrary Operator where
    arbitrary :: Gen Operator
    arbitrary = arbitraryBoundedEnum

-- Implement parsing something in terms of its show output.
parseShow :: Show a => a -> Parser a
parseShow tok = string (show tok) $> tok

-- We'll implement operator parsing in terms of parseShow and the very helpful deriving Bounded!
parseOp' :: Parser Operator
parseOp' = choice $ parseShow <$> [minBound .. maxBound]

parseOp :: Parser Token
parseOp = Op <$> parseOp'

parseSemi :: Parser Token
parseSemi = char ';' $> Semicolon

parseScopeOpen :: Parser Token
parseScopeOpen = char '{' $> ScopeOpen

parseScopeClose :: Parser Token
parseScopeClose = char '}' $> ScopeClose

-- this is actually weirdly fun
parseToken :: Parser Token
parseToken = choice [parseIdentKW,
                     FloatConst <$> parseFloat,
                     IntConst <$> (read <$> some digit),
                     parseOp,
                     parseSemi,
                     parseScopeOpen,
                     parseScopeClose]

-- Parse as many tokens as we can. Ignore all preceding spaces before a token
-- Ensure that the token stream ends with the end of file! No extra tokens at the end.
parseTokens :: Parser [Token]
parseTokens = many (many space *> parseToken) <* many space <* eof

tokenize :: String -> Either ParseError [Token]
tokenize = parse parseTokens

tokenizeOrDie :: String -> [Token]
tokenizeOrDie s = case tokenize s of
    Left e -> error e
    Right ts -> ts

tokenizeFromFile :: String -> IO (Either ParseError [Token])
tokenizeFromFile = parseFromFile parseTokens

tokenizeFromFileOrDie :: String -> IO [Token]
tokenizeFromFileOrDie s = do
    ps <- tokenizeFromFile s

    case ps of
        Left e -> error e
        Right ts -> return ts


data Token = KW Keyword
           | Identifier {getIdent :: String}
           | IntConst {getInt :: Int}
           | FloatConst {getFloat :: Float}
           | Op Operator
           | Semicolon
           | ScopeOpen
           | ScopeClose deriving (Show, Eq)

instance Arbitrary Token where
    arbitrary :: Gen Token
    arbitrary = oneof [
                KW <$> arbitrary,
                -- Not just any string will do, we need one that follows the format of an identifier
                Identifier <$> genIdent,
                -- Note that negative numbers are not single tokens.
                -- They're tokenized as a unary minus followed by a numeric literal
                IntConst <$> (getNonNegative <$> (arbitrary :: Gen (NonNegative Int))),
                FloatConst <$> (getNonNegative <$> (arbitrary :: Gen (NonNegative Float))),
                Op <$> arbitrary,
                return Semicolon,
                return ScopeOpen,
                return ScopeClose
            ]

type TParser = GenParser Token

dumpToken :: Token -> String
dumpToken (KW k)         = fromMaybe "#BADTOKEN#" $ List.stripPrefix "KW_" (show k)
dumpToken (Identifier i) = i
dumpToken (IntConst i)   = show i
dumpToken (FloatConst f) = show f
dumpToken (Op o)         = show o
dumpToken Semicolon      = ";"
dumpToken ScopeOpen      = "{"
dumpToken ScopeClose     = "}"

dumpTokenList :: [Token] -> String
dumpTokenList ts = unwords $ dumpToken <$> ts

-- Ensure our tokenizer is correct by randomly generating a list of tokens,
-- dumping it to a string, and seeing if our parser correctly returns the same list of tokens
prop_tokenize :: [Token] -> Bool
prop_tokenize ts = case tokenize $ dumpTokenList ts of
    Left x -> False
    Right ts' -> ts == ts'
