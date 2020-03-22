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

-- associativity of operators in reverse precedence order
%nonassoc '>' '>=' '<' '<=' '==' '!='
%left '+' '-'
%left '*' '/'
%nonassoc ':='
%%

-- Entry point
Program :: {Program}
    : ProgramHead VarDecBlock Block { $3 }

ProgramHead :: {String}
    : 'program' ID ';' { $2 }

VarDecBlock :: {VarDecBlock}
    : 'var' ID_List ':' Type ';' { VarDecBlock $2 $4 }

ID_List :: {[String]}
    : ID { [] }
    | ID ',' ID_List { $1 : $3 }

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

{}
