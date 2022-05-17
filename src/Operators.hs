-- Defines types for all operators, as well as handy shorthand token -> operator parsers
module Operators where

import Lex
import ParserCombinators
import Data.Functor

-- Prefix unary operation
data UnaryOp = Increment | Decrement | Plus | Minus | Invert | Not
    deriving (Show, Eq)

-- Postfix unary operation
data PostfixOp = PostIncrement | PostDecrement
    deriving (Show, Eq)

-- As stupid as these look, I'm actually really happy with how well they work
-- inside of the ParseTree module

(#++^) :: TParser UnaryOp
(#++^) = char (Op O_PlusPlus) $> Increment

(#--^) :: TParser UnaryOp
(#--^) = char (Op O_MinusMinus) $> Decrement

(#+^) :: TParser UnaryOp
(#+^) = char (Op O_Plus) $> Plus

(#-^) :: TParser UnaryOp
(#-^) = char (Op O_Minus) $> Minus

(#~^) :: TParser UnaryOp
(#~^) = char (Op O_Tilde) $> Invert

(#!^) :: TParser UnaryOp
(#!^) = char (Op O_Not) $> Not

(#^++) :: TParser PostfixOp
(#^++) = char (Op O_PlusPlus) $> PostIncrement

(#^--) :: TParser PostfixOp
(#^--) = char (Op O_MinusMinus) $> PostDecrement

data BinOp = LessEqual | GreaterEqual | Less | Greater | Equal | NotEqual
           | And | Xor | Or | LAnd | LXor | LOr
           | Add | Sub | Mult | Div | Mod
           | LShift | RShift | Comma
    deriving (Show, Eq)

(#||) :: TParser BinOp
(#||) = char (Op O_LOr) $> LOr

(#^^) :: TParser BinOp
(#^^) = char (Op O_LXor) $> LXor

(#&&) :: TParser BinOp
(#&&) = char (Op O_LAnd) $> LAnd

(#|) :: TParser BinOp
(#|) = char (Op O_LOr) $> LOr

(#^) :: TParser BinOp
(#^) = char (Op O_LXor) $> LXor

(#&) :: TParser BinOp
(#&) = char (Op O_And) $> And

(#==) :: TParser BinOp
(#==) = char (Op O_EQ) $> Equal

(#!=) :: TParser BinOp
(#!=) = char (Op O_NE) $> NotEqual

(#<=) :: TParser BinOp
(#<=) = char (Op O_LE) $> LessEqual

(#>=) :: TParser BinOp
(#>=) = char (Op O_GE) $> GreaterEqual

(#<) :: TParser BinOp
(#<) = char (Op O_LT) $> Less

(#>) :: TParser BinOp
(#>) = char (Op O_GT) $> Greater

(#<<) :: TParser BinOp
(#<<) = char (Op O_LShift) $> LShift

(#>>) :: TParser BinOp
(#>>) = char (Op O_RShift) $> RShift

(#+) :: TParser BinOp
(#+) = char (Op O_Plus) $> Add

(#-) :: TParser BinOp
(#-) = char (Op O_Minus) $> Sub

(#*) :: TParser BinOp
(#*) = char (Op O_Mult) $> Mult

(#/) :: TParser BinOp
(#/) = char (Op O_Div) $> Div

(#%) :: TParser BinOp
(#%) = char (Op O_Mod) $> Mod


-- data Operator = O_PlusPlus | O_MinusMinus | O_LShift | O_RShift 
--               | O_LE | O_GE | O_EQ | O_NE | O_LAnd | O_LXor | O_LOr 
--               | O_PlusAssign | O_MinusAssign | O_MultAssign | O_DivAssign | O_ModAssign 
--               | O_LParen | O_RParen | O_LBracket | O_RBracket | O_Dot 
--               | O_Plus | O_Minus | O_Tilde | O_Not | O_Mult | O_Div | O_Mod 
--               | O_LT | O_GT | O_And | O_Xor | O_Or 
--               | O_QMark | O_Colon | O_Assign | O_Comma

data Assignment = Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign
    deriving (Show, Eq)
