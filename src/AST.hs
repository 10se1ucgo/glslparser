-- The main AST data structure.
-- All GLSL source files are parsed as a TranslationUnit, i.e. a list of
-- function and variable definitions.

module AST (QualifiedType,
            FunctionArgument (..),
            Statement (..),
            Expr (..),
            CompoundStatement (..),
            Decl (..),
            TranslationUnit (..))
    where

import qualified TypeSpecifiers as T
import qualified Operators as O

-- Types can be qualified by a list of qualifiers (const, uniform, in, out, etc...)
-- Not going to support it (yet), la-mow
type QualifiedType = T.TypeSpecifier

data Statement = EmptyStatement
               | DeclStatement Decl
               | ExprStatement Expr
               | IfStatement Expr Statement (Maybe Statement)
               | StatementList CompoundStatement
    deriving (Show, Eq)

newtype CompoundStatement = CompoundStatement [Statement]
    deriving (Show, Eq)

data Expr = VarExpr String
          | IntExpr Int
          | FloatExpr Float
          | UnaryExpr O.UnaryOp Expr
          | PostfixExpr Expr O.PostfixOp
          | BinExpr O.BinOp Expr Expr
          | AssignExpr O.Assignment Expr Expr
          | ConditionExpr Expr Expr Expr
          | FuncCallExpr String [Expr]
    deriving (Show, Eq)

data FunctionArgument = FunctionArgument QualifiedType (Maybe String)
    deriving (Eq)

instance Show FunctionArgument where
   show (FunctionArgument qtype (Just name)) = T.typeConstructorName qtype ++ " " ++ name
   show (FunctionArgument qtype Nothing) = T.typeConstructorName qtype ++ " <unnamed>"

data Decl = FuncDecl QualifiedType String [FunctionArgument] CompoundStatement
          | VarsDecl QualifiedType String (Maybe Expr)
    deriving (Eq)

instance Show Decl where
   show (FuncDecl rtype fname args (CompoundStatement statements)) =
        unwords ["FunctionDecl<#" ++ T.typeConstructorName rtype,
                 fname ++ show args,
                 show statements ++ "#>"]
   show (VarsDecl qtype vname (Just expr)) =
        unwords ["VarDecl<#" ++ T.typeConstructorName qtype,
                 vname,
                 "=",
                 show expr ++ "#>"]
   show (VarsDecl qtype vname Nothing) =
        unwords ["VarDecl<#" ++ T.typeConstructorName qtype,
                 vname ++ "#>"]

newtype TranslationUnit = TranslationUnit [Decl]
    deriving (Show, Eq)
