-- This file contains the data-structures for the AST
-- The role of the parser is to build the AST (Abstract Syntax Tree) 

module Pascal.Data
    (
        VarDecBlock(..),
        VarDec_List(..),
        Type(..),
        Exp(..),
        BoolExp(..),
        Statement(..),
        Program
    ) where

data VarDecBlock = 
    VarDecBlock [VarDec_List]

data VarDec_List =
    VarDec_List [String] Type

data Type = BOOLEAN | REAL

-- Data-structure for  numeric expressions
data Exp = 
    -- unary operator: Op name expression
    Op1 String Exp
    -- binary operator: Op name leftExpression rightExpression
    | Op2 String Exp Exp
    | Op3 String Exp
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
    | If String BoolExp String Statement
    | Fun String String [VarDec_List] String Statement String
    | Proc String String [VarDec_List] String Statement String

-- Data-structure for whole program
-- TODO: add declarations and other useful stuff
-- Hint: make a tuple containing the other ingredients
type Program = [Statement]
