# glslparser

An (incomplete) parser for GLSL shader programs, written in Haskell. 

Final project submission for CMSC488b, Advanced Functional Programming.

# Running

Try doing `stack run <file_name_here>`. 

As an example, this repository contains two files `shaders/test1.vert` and `shaders/test2.vert` 

`stack run shaders/test1.vert`, for example, will print a semi-pretty representation of the AST
parsed from this file.

# Functionality

Currently, this project only really just achieves parsing and AST printing.
I did have higher hopes for my project submission, including pretty-printing (code formatting),
optimization, and even limited execution at some point.

While I unfortunately just didn't have enough time (or skill) to make that happen,
I'm still really happy with the result. I've gotten a taste of practically applying Haskell
to one of its iconic domains, and have gotten insight in using things like Monads and Applicative.
Parsing in Haskell using parser combinators is truly a joy compared to many other languages, 
and I've learned patterns I'll never be able to unsee!

# Code Overview

First, the program input string is tokenized into discrete tokens, by the `Lex` module, of the form:

```hs
-- From Lex.hs
data Token = KW Keyword
           | Identifier String
           | IntConst Int
           | FloatConst Float
           | Op Operator
           | Semicolon
           | ScopeOpen
           | ScopeClose
```

Why? Couldn't we just parse the code directly? Yes! But, the
[GLSL specification](https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.3.30.pdf)
is written in terms of discrete tokens, and it just makes everything a lot easier when I
can follow the spec 1 to 1!

This is then parsed into a nice AST by the `ParseTree` module, with the structure
of the AST being defined in the `AST` module. Abbreviated definition is as given:


```hs
-- From AST.hs
-- The ultimate structure, the parent of everything, representing a single source file
newtype TranslationUnit = TranslationUnit [Decl]

data FunctionArgument = FunctionArgument QualifiedType (Maybe String)

data Decl = FuncDecl QualifiedType String [FunctionArgument] CompoundStatement
          | VarsDecl QualifiedType String (Maybe Expr)

newtype CompoundStatement = CompoundStatement [Statement]

data Statement = EmptyStatement
               | DeclStatement Decl
               | ExprStatement Expr
               | IfStatement Expr Statement (Maybe Statement)
               | StatementList CompoundStatement

data Expr = VarExpr String
          | IntExpr Int
          | FloatExpr Float
          | UnaryExpr O.UnaryOp Expr
          | PostfixExpr Expr O.PostfixOp
          | BinExpr O.BinOp Expr Expr
          | AssignExpr O.Assignment Expr Expr
          | ConditionExpr Expr Expr Expr
          | FuncCallExpr String [Expr]
```

# Limitations

As is likely plainly obvious, this parser supports a very limited subset of the GLSL language.

For example, we're missing:  
- The preprocessor
- Type qualifiers! (`const`, `uniform`, `in`, `out`, `centroid`, etc...)
- User-defined types (i.e. structs) and uniform blocks
- Various different statements (while, do-while, switch/case, etc.)
- Member accessors and vector swizzling

With time, I hope to add support for these!

# Testing

A Quick Check property can be found in `Lex.hs`. It essentialy randomly generates
a whole bunch of tokens, dumps it back to its string form, and checks to make sure
they are tokenized in the exact same way.