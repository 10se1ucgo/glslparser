-- Uses the Data.Tree module to semi-pretty print the AST
module SemiPrint (printTU) where

import Data.Tree
import AST
import TypeSpecifiers as T

printTU :: TranslationUnit -> String
printTU (TranslationUnit decls) = drawTree (Node "TranslationUnit" (printDecl <$> decls))

printDecl :: Decl -> Tree String
printDecl (FuncDecl rtype fname args (CompoundStatement statements)) =
    Node (unwords ["FunctionDecl: " ++ T.typeConstructorName rtype, fname ++ show args])
         (printStatement <$> statements)
printDecl (VarsDecl qtype vname (Just expr)) =
    Node (unwords ["VarDecl: " ++ T.typeConstructorName qtype, vname]) [printExpr expr]
printDecl (VarsDecl qtype vname Nothing) =
    Node (unwords ["VarDecl: " ++ T.typeConstructorName qtype, vname]) []

printStatement :: Statement -> Tree String
printStatement EmptyStatement = Node "EmptyStatement" []
printStatement (DeclStatement d) = Node "DeclStatement" [printDecl d]
printStatement (ExprStatement e) = Node "ExprStatement" [printExpr e]
printStatement (IfStatement e t (Just f)) = Node "IfStatement" [printExpr e, printStatement t, printStatement f]
printStatement (IfStatement e t Nothing) = Node "IfStatement" [printExpr e, printStatement t]
printStatement (StatementList (CompoundStatement cs)) = Node "CompoundStatement" (printStatement <$> cs)

printExpr :: Expr -> Tree String
printExpr (VarExpr v) = Node ("VarExpr: " ++ v) []
printExpr (IntExpr i) = Node ("IntExpr: " ++ show i) []
printExpr (FloatExpr f) = Node ("FloatExpr: " ++ show f) []
printExpr (UnaryExpr o e) = Node ("UnaryExpr " ++ show o) [printExpr e]
printExpr (PostfixExpr e o) = Node ("PostfixExpr " ++ show o) [printExpr e]
printExpr (BinExpr o e1 e2) = Node ("BinExpr " ++ show o) [printExpr e1, printExpr e2]
printExpr (AssignExpr a e1 e2) = Node ("AssignExpr " ++ show a) [printExpr e1, printExpr e2]
printExpr (ConditionExpr c e1 e2) = Node "ConditionExpr" [printExpr c, printExpr e1, printExpr e2]
printExpr (FuncCallExpr fname args) = Node ("FuncCallExpr: " ++ show fname) (printExpr <$> args)
