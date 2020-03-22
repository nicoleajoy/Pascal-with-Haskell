-- This file contains the data-structures for the AST
-- The role of the parser is to build the AST (Abstract Syntax Tree) 

module Pascal.Data
    (
        VarDecBlock(..),
        Type(..),
        Exp(..),
        BoolExp(..),
        Statement(..),
        Program
    ) where

data VarDecBlock = 
    VarDecBlock [String] Type

data Type = BOOLEAN | REAL

-- Data-structure for  numeric expressions
data Exp = 
    -- unary operator: Op name expression
    Op1 String Exp
    -- binary operator: Op name leftExpression rightExpression
    | Op2 String Exp Exp
    -- function call: FunctionCall name ListArguments
    | FunCall String [Exp]
    -- real value: e.g. Real 1.0
    | Real Float
    -- variable: e.g. Var "x"
    | Var_R String

-- Data-structure for boolean expressions
data BoolExp = 
    -- binary operator on boolean expressions
    OpB String BoolExp BoolExp
    -- negation, the only unary operator
    | Not BoolExp
    -- comparison operator: Comp name expression expression
    | Comp String Exp Exp
    -- true and false constants
    | True_C | False_C
    -- variable: e.g. Var "x"
    | Var_B String

-- Data-structure for statements
data Statement = 
    -- TODO: add other statements
    -- Variable assignment
    Assign String Exp
    -- If statement
    | If BoolExp Statement Statement

-- Data-structure for whole program
-- TODO: add declarations and other useful stuff
-- Hint: make a tuple containing the other ingredients
type Program = [Statement]