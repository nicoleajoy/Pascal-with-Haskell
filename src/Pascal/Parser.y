{
module Pascal.Parser where

import Pascal.Base
import Pascal.Data
import Pascal.Lexer
}


%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TokenEOF }

%token
        float           { Token _ (TokenFloat $$) }
        ID              { Token _ (TokenID $$) }
        '+'             { Token _ (TokenOp "+") }
        '-'             { Token _ (TokenOp "-") }
        '*'             { Token _ (TokenOp "*") }
        '/'             { Token _ (TokenOp "/") }
        '='             { Token _ (TokenOp "=") }
        '('             { Token _ (TokenK  "(") }
        ')'             { Token _ (TokenK  ")") }
        '.'             { Token _ (TokenK ".") }
        ','             { Token _ (TokenK ",") }
        ':'             { Token _ (TokenK ":") }
        ';'             { Token _ (TokenK ";") }
        'program'       { Token _ (TokenK "program") }
        'begin'         { Token _ (TokenK "begin") }
        'end'           { Token _ (TokenK "end") }
        'var'           { Token _ (TokenK "var") }
        'boolean'       { Token _ (TokenK "boolean") }
        'real'          { Token _ (TokenK "real") }
        ':='            { Token _ (TokenK ":=") }
        'true'          { Token _ (TokenK "true") }
        'false'         { Token _ (TokenK "false") }
        'and'           { Token _ (TokenK "and") }
        'or'            { Token _ (TokenK "or") }
        'not'           { Token _ (TokenK "not") }
        'sin'           { Token _ (TokenK "sin") }
        'cos'           { Token _ (TokenK "cos") }
        'exp'           { Token _ (TokenK "exp") }
        'ln'            { Token _ (TokenK "ln") }
        'sqrt'          { Token _ (TokenK "sqrt") }
        'writeln'       { Token _ (TokenK "writeln") }
        'readln'        { Token _ (TokenK "readln") }
        'for'           { Token _ (TokenK "for") }
        'if'            { Token _ (TokenK "if") }
        'then'          { Token _ (TokenK "then") }
        'else'          { Token _ (TokenK "else") }
        'while'         { Token _ (TokenK "while") }
        'function'      { Token _ (TokenK "function") }
        'procedure'     { Token _ (TokenK "procedure") }

-- associativity of operators in reverse precedence order
%nonassoc '>' '>=' '<' '<=' '==' '!='
%left '+' '-'
%left '*' '/'
%nonassoc ':='
%%

-- Entry point
Program :: {Program}
    : ProgramHeader VarDecBlock Block { $3 }

ProgramHeader :: {String}
    : 'program' ID ';' { $2 }

VarDecBlock :: {[VarDec_List]}
    : { [] } -- nothing; empty list
    | 'var' VarDec_Lists { $2 }

VarDec_Lists :: {[VarDec_List]}
    : VarDec_List { [$1] }
    | VarDec_List VarDec_Lists { $1:$2 }

VarDec_List :: {VarDec_List}
    : ID_List ':' Type ';' { VarDec_List $1 $3 }

ID_List :: {[String]}
    : ID { [] }
    | ID ',' ID_List { $1:$3 }

Block :: {[Statement]}
    : 'begin' Statements 'end' '.' { $2 }

Type :: {Type}
    : 'boolean' { BOOLEAN }
    | 'real' { REAL }

-- Expressions
Exp :: {Exp}
    : '+' Exp { $2 } -- ignore Plus
    | '-' Exp { Op1 "-" $2 }
    | Exp '+' Exp { Op2 "+" $1 $3 }
    | Exp '*' Exp { Op2 "*" $1 $3 }
    | Exp '/' Exp { Op2 "/" $1 $3 }
    | 'sin' '(' Exp ')' {Op3 "sin" $3}
    | 'cos' '(' Exp ')' {Op3 "cos"$3}
    | 'exp' '(' Exp ')' {Op3 "exp" $3}
    | 'ln' '(' Exp ')' {Op3 "ln" $3}
    | 'sqrt' '(' Exp ')' {Op3 "sqrt" $3}
    | '(' Exp ')' { $2 } -- ignore brackets
    | float { Real $1 }
    | ID { Var_R $1 }
  

BoolExp :: {BoolExp}
    : 'true' { True_C }
    | 'false' { False_C }
    | 'not' BoolExp { Not $2 }
    | BoolExp 'and' BoolExp { OpB "and" $1 $3 }
    | BoolExp 'or' BoolExp { OpB "or" $1 $3 }
    | ID { Var_B $1 }

Statements :: {[Statement]}
    : { [] } -- nothing; make empty list
    | Statement Statements { $1:$2 } -- add statement to list
  

Statement :: {Statement}
    : ID ':=' Exp { Assign $1 $3 }
    | 'if' BoolExp 'then' Statement { If "if" $2 "then" $4 }
    | 'function' ID VarDec_List 'begin' Statement 'end' ';' {Fun "function" $2 [$3] "begin" $5 "end"}
    | 'procedure' ID VarDec_List 'begin' Statement 'end' ';' {Proc "procedure" $2 [$3] "begin" $5 "end"}
    

{}
