module TypeSpecifiers where

import Data.Char

-- This is really stupid, but works.

data TypeSpecifier = Void | Int | Float | Bool | Vec2 | Vec3 | Vec4 | Mat2 | Mat3 | Mat4
    deriving (Show, Eq)

typeConstructorName :: TypeSpecifier -> String
typeConstructorName s = toLower <$> show s