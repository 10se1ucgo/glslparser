-- Takes in Token list from Lex, parses it into a nice AST (as defined in the AST module)
module ParseTree where

import ParserCombinators
import Lex
import AST
-- Avoid constructor names clashing
import qualified TypeSpecifiers as T
import qualified Operators as O
-- Import for the (#op) parsers
import Operators hiding (UnaryOp (..), PostfixOp (..), BinOp (..), Assignment (..))

import Control.Applicative
import Data.Char
import Data.Functor


parseTypeSpec :: TParser QualifiedType
parseTypeSpec = choice [
        char (KW KW_void ) $> T.Void,
        char (KW KW_int  ) $> T.Int,
        char (KW KW_float) $> T.Float,
        char (KW KW_bool ) $> T.Bool,
        char (KW KW_vec2 ) $> T.Vec2,
        char (KW KW_vec3 ) $> T.Vec3,
        char (KW KW_vec4 ) $> T.Vec4,
        char (KW KW_mat2 ) $> T.Mat2,
        char (KW KW_mat3 ) $> T.Mat3,
        char (KW KW_mat4 ) $> T.Mat4
    ]

parseAssignOp :: TParser O.Assignment
parseAssignOp = choice [
        char (Op O_Assign     ) $> O.Assign,
        char (Op O_PlusAssign ) $> O.AddAssign,
        char (Op O_MinusAssign) $> O.SubAssign,
        char (Op O_MultAssign ) $> O.MultAssign,
        char (Op O_DivAssign  ) $> O.DivAssign,
        char (Op O_ModAssign  ) $> O.ModAssign
    ]

-- This is really scuffed but whatever!
-- We use this to parse tokens that have inner data and extract the data,
-- using `satisfy` to guarantee the token is the proper type.
isIdentifier :: Token -> Bool
isIdentifier (Identifier _) = True
isIdentifier _ = False

isIntConst :: Token -> Bool
isIntConst (IntConst _) = True
isIntConst _ = False

isFloatConst :: Token -> Bool
isFloatConst (FloatConst _) = True
isFloatConst _ = False

parseIdentifier :: TParser String
parseIdentifier = getIdent <$> satisfy isIdentifier

parseIntConst :: TParser Int
parseIntConst = getInt <$> satisfy isIntConst

parseFloatConst :: TParser Float
parseFloatConst = getFloat <$> satisfy isFloatConst

-- Parse anything between two paren tokens
betweenParens :: TParser a -> TParser a
betweenParens x = between (char $ Op O_LParen) x (char $ Op O_RParen)

-- Function prototype argument is a type_name var_name?
parseFuncArg :: TParser FunctionArgument
parseFuncArg = FunctionArgument <$> parseTypeSpec <*> optional parseIdentifier

-- Comma separated list of func prototype args
parseFuncArgList :: TParser [FunctionArgument]
parseFuncArgList = sepBy parseFuncArg (char $ Op O_Comma)

-- return_type func_name(arg_list...) { <statements> }
parseFuncDecl :: TParser Decl
parseFuncDecl = FuncDecl
            <$> parseTypeSpec
            <*> parseIdentifier
            <*> betweenParens parseFuncArgList
            <*> parseCompoundStatement

-- Parse the optional (= <expr>)? part of an assignment
parseVarInit :: TParser (Maybe Expr)
parseVarInit = (char (Op O_Assign) *> (Just <$> parseExpr)) <|> pure Nothing

-- type_name var_name (= <expr>)? ;
parseVarDecl :: TParser Decl
parseVarDecl = VarsDecl
           <$> parseTypeSpec
           <*> parseIdentifier
           <*> parseVarInit
           <*  char Semicolon

parseDecl :: TParser Decl
parseDecl = parseFuncDecl <|> parseVarDecl

-- Highest level expression
-- expr = assignment_expr | expr COMMA assignment_expr
parseExpr :: TParser Expr
parseExpr = parseAssignExpr -- TODO: Comma expr!

-- Note: Assignment is right-associative
-- assignment_expr = condition_expr | unary_expr ASSIGN_OP assign_expr
parseAssignExpr :: TParser Expr
parseAssignExpr = (flip AssignExpr
               <$> parseUnaryExpr
               <*> parseAssignOp
               <*> parseAssignExpr) <|> parseConditionExpr

-- Note: ?: is right-associative
-- cond_expr = logical_or_expr QMARK expr COLON assign_expr
parseConditionExpr :: TParser Expr
parseConditionExpr = (ConditionExpr
                  <$> parseLogicalOrExpr
                  <*  char (Op O_QMark)
                  <*> parseExpr
                  <*  char (Op O_Colon)
                  <*> parseAssignExpr) <|> parseLogicalOrExpr

-- This feels like a good case for some good ol fashioned generated code, or maybe template haskell,
-- but I don't know enough about it to feasibly implement it in time

-- These all follow the format of (a_expr = a_expr OP b_expr | b_expr)
-- Where b_expr is the parser for the next operator by precedence
-- And OP is either a single operator, or a list of operators of the same precedence
-- These are all left associative, and therefore use the chainl operation.
parseLogicalOrExpr  :: TParser Expr
parseLogicalOrExpr  = chainl1 parseLogicalXorExpr $ BinExpr <$> (#||)

parseLogicalXorExpr :: TParser Expr
parseLogicalXorExpr = chainl1 parseLogicalAndExpr $ BinExpr <$> (#^^)

parseLogicalAndExpr :: TParser Expr
parseLogicalAndExpr = chainl1 parseBitOrExpr      $ BinExpr <$> (#&&)

parseBitOrExpr      :: TParser Expr
parseBitOrExpr      = chainl1 parseBitXorExpr     $ BinExpr <$> (#|)

parseBitXorExpr     :: TParser Expr
parseBitXorExpr     = chainl1 parseBitAndExpr     $ BinExpr <$> (#^)

parseBitAndExpr     :: TParser Expr
parseBitAndExpr     = chainl1 parseEqualityExpr   $ BinExpr <$> (#&)

parseEqualityExpr   :: TParser Expr
parseEqualityExpr   = chainl1 parseRelationExpr   $ BinExpr <$> ((#==) <|> (#!=))

parseRelationExpr   :: TParser Expr
parseRelationExpr   = chainl1 parseShiftExpr      $ BinExpr <$> choice [(#<), (#>), (#<=), (#>=)]

parseShiftExpr      :: TParser Expr
parseShiftExpr      = chainl1 parseAddExpr        $ BinExpr <$> ((#<<) <|> (#>>))

parseAddExpr        :: TParser Expr
parseAddExpr        = chainl1 parseMultExpr       $ BinExpr <$> ((#+) <|> (#-))

parseMultExpr       :: TParser Expr
parseMultExpr       = chainl1 parseUnaryExpr      $ BinExpr <$> choice [(#*), (#/), (#%)]

parseUnaryExpr :: TParser Expr
parseUnaryExpr = (UnaryExpr
              <$> choice [(#++^), (#--^), (#+^), (#-^), (#~^), (#!^)]
              <*> parseUnaryExpr) <|> parsePostfixExpr

-- TODO: Actual postfix operators!!! I'm having trouble thinking how to implement it, but it shouldn't be hard.
parsePostfixExpr :: TParser Expr
parsePostfixExpr = parseFuncCall <|> parsePrimaryExpr

-- Parses a terminal expression (a constant or variable), or an expression surrounded by parens
parsePrimaryExpr :: TParser Expr
parsePrimaryExpr = choice [
        VarExpr <$> parseIdentifier,
        IntExpr <$> parseIntConst,
        FloatExpr <$> parseFloatConst,
        betweenParens parseExpr
    ]

-- Function names are either an identifier or the name of a type constructor (i.e. vec3(float, float, float))
parseFunctionName :: TParser String
parseFunctionName = choice [
        parseIdentifier,
        T.typeConstructorName <$> parseTypeSpec
    ]

-- Func args are just comma separated expressions.
-- Specifically assign_expr and not general expr since we don't want to parse the comma operator here!!!
-- That'd be evil and bad!
parseFuncCallArgs :: TParser [Expr]
parseFuncCallArgs = sepBy parseAssignExpr (char $ Op O_Comma)

-- A function call is just a func_name(func_args)
parseFuncCall :: TParser Expr
parseFuncCall = FuncCallExpr <$> parseFunctionName <*> betweenParens parseFuncCallArgs

-- An optional expression followed by a semicolon
parseExprStatement :: TParser Statement
parseExprStatement = ExprStatement 
                 <$> (parseExpr <* char Semicolon) <|> (char Semicolon $> EmptyStatement)

-- Parse else <body> or return Nothing 
parseElseStatement :: TParser (Maybe Statement)
parseElseStatement = (char (KW KW_else) *> (Just <$> parseStatement)) <|> pure Nothing

-- if (<expr>) <statement> (else <statement>)?
parseIfStatement :: TParser Statement
parseIfStatement = char (KW KW_if) *> (IfStatement 
                                   <$> betweenParens parseExpr 
                                   <*> parseStatement 
                                   <*> parseElseStatement)

parseStatement :: TParser Statement
parseStatement = choice [StatementList <$> parseCompoundStatement, DeclStatement <$> parseVarDecl, parseIfStatement, parseExprStatement]

-- A list of statements surrounded by scope symbols { }
parseCompoundStatement :: TParser CompoundStatement
parseCompoundStatement = between (char ScopeOpen) (CompoundStatement <$> many parseStatement) (char ScopeClose)

-- The ultimate parser!!! Parses an entire translation unit (i.e.: a list of function and var definitions)
parseTU :: TParser TranslationUnit
parseTU = (TranslationUnit <$> many parseDecl) <* eof

parseTUOrDie :: [Token] -> TranslationUnit
parseTUOrDie ts = case parse parseTU ts of
    Left e -> error e
    Right tu -> tu
