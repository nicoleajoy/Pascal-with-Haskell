module Pascal.Interpret 
(
    interpret
)
where

import Pascal.Data
import qualified Data.Map.Strict()
import Prelude hiding (map) 
import Control.Monad.Cont
import Data.Traversable
import qualified Data.Map as Map

-- TODO: define auxiliary functions to aid interpretation
-- Feel free to put them here or in different modules
-- Hint: write separate evaluators for numeric and
-- boolean expressions and for statements

-- make sure you write test unit cases for all functions
map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs  

interpret :: Program -> String
-- TODO: write the interpreter
--interpret (x) = show( map state x)
interpret (x) = show( map state x )

state :: Statement -> String
state (AssignR var x) = getExp x
state (AssignB var bool) = booExp bool
state (IfThen bool statement) = state statement
state _ = "empty list"


getExp :: Exp -> String
getExp (Op1 "+" exp) = getExp exp
getExp (Op2 "+" (Real exp) (Real exp1)) = show (exp + exp1)
getExp (Op2 "*" (Real exp) (Real exp1)) = show (exp + exp1)
getExp (Op2 "-" (Real exp) (Real exp1)) = show (exp + exp1)
getExp (Op2 "/" (Real exp) (Real exp1)) = show (exp + exp1)
getExp (Op3 "sin" (Real op)) = show (sin op)
getExp (Op3 "cos" (Real op)) = show (cos (op)) 
getExp (Op3 "ln" (Real op)) = show (log(op))
getExp (Op3 "exp" (Real op)) = show op
getExp (Op3 "sqrt" (Real op)) = show (sqrt op)
getExp (FunCall text [exp]) = "function"
getExp (Real num) = show num
getExp (Var_R text) = show text
getExp _ = "not exp"


-- eval ::Exp ->String
-- eval (exp,exp1) = show exp


booExp :: BoolExp -> String
booExp (OpB text bool bool1) = show bool ++ (show bool1)
booExp (Not bool) = show bool
booExp (OpR text exp exp1) = show exp ++ (show exp1)
booExp (True_C) = "true"
booExp (False_C) = "false"
booExp (Var_B text) = show text
booExp _ = "not boo"

-- getType :: Type -> Float
-- getType (REAL) 