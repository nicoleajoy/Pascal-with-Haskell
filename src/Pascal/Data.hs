-- This file contains the data-structures for the AST
-- The role of the parser is to build the AST (Abstract Syntax Tree) 

module Pascal.Data
    (
        VarDecBlock(..),
        VarDec(..),
        Type(..),
        Exp(..),
        BoolExp(..),
        Statement(..),
        CaseLabel(..),
        WriteParam(..),
        Program
    ) where

data VarDecBlock = 
    VarDecBlock [VarDec]
    deriving (Show, Eq)

data VarDec =
    VarDec [String] Type
    | VarDef_Float String Float
    | VarDef_True String
    | VarDef_False String
    deriving (Show, Eq)

data Type = 
    BOOLEAN 
    | REAL
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data BoolExp = 
    -- operator on boolean expressions
    OpB String BoolExp BoolExp
    -- negation, the only unary operator
    | Not BoolExp
    -- operator on real expressions
    | OpR String Exp Exp
    -- true and false constants
    | True_C 
    | False_C
    -- variable: e.g. Var "x"
    | Var_B String
    deriving (Show, Eq)

data Statement = 
    -- assign float
    AssignR String Exp
    -- assign boolean
    | AssignB String BoolExp
    | IfThen BoolExp Statement
    | IfThenElse BoolExp Statement Statement
    | WhileDo BoolExp [Statement]
    | ForDo String Float Float [Statement]
    | Case Exp [CaseLabel]
    -- write() with no parameters
    | WriteNewLine
    -- write(stuff, inside) with parameters
    | WriteInside [WriteParam]
    -- TODO: add function and procedure blocks
    | Func String String [VarDec] String Statement String
    | Proc String String [VarDec] String Statement String
    deriving (Show, Eq)

data CaseLabel = 
    CaseLabel Exp Statement
    deriving (Show, Eq)

data WriteParam = 
    PrintID String
    | PrintTrue
    | PrintFalse
    | PrintFloat Float
    | PrintString String
    deriving (Show, Eq)

-- Data-structure for whole program
-- TODO: add declarations and other useful stuff
-- Hint: make a tuple containing the other ingredients
type Program = [Statement]
