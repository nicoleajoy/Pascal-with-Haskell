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
        'true'          { Token _ (TokenBool true) }
        'false'         { Token _ (TokenBool false) }
        '+'             { Token _ (TokenOp "+") }
        '-'             { Token _ (TokenOp "-") }
        '*'             { Token _ (TokenOp "*") }
        '/'             { Token _ (TokenOp "/") }
        'mod'           { Token _ (TokenOp "mod") }
        '>'             { Token _ (TokenOp ">") }
        '>='            { Token _ (TokenOp ">=") }
        '<'             { Token _ (TokenOp "<") }
        '<='            { Token _ (TokenOp "<=") }
        '='             { Token _ (TokenOp "=") }
        '<>'            { Token _ (TokenOp "<>") }
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
        'and'           { Token _ (TokenK "and") }
        'or'            { Token _ (TokenK "or") }
        'not'           { Token _ (TokenK "not") }
        'sin'           { Token _ (TokenK "sin") }
        'cos'           { Token _ (TokenK "cos") }
        'exp'           { Token _ (TokenK "exp") }
        'ln'            { Token _ (TokenK "ln") }
        'sqrt'          { Token _ (TokenK "sqrt") }
        'if'            { Token _ (TokenK "if") }
        'then'          { Token _ (TokenK "then") }
        'else'          { Token _ (TokenK "else") }
        'while'         { Token _ (TokenK "while") }
        'do'            { Token _ (TokenK "do") }
        'for'           { Token _ (TokenK "for") }
        'to'            { Token _ (TokenK "to") }
        'case'          { Token _ (TokenK "case") }
        'of'            { Token _ (TokenK "of") }
        'writeln'       { Token _ (TokenK "writeln") }
        'readln'        { Token _ (TokenK "readln") }
        'function'      { Token _ (TokenK "function") }
        'procedure'     { Token _ (TokenK "procedure") }

-- associativity of operators in reverse precedence order
%nonassoc '>' '>=' '<' '<=' '=' '<>'
%nonassoc ':='
%left '+' '-'
%left '*' '/'
%%

-- Entry point
Program :: {Program}
    : ProgramHeader VarDecBlock Block { $3 }

ProgramHeader :: {String}
    : 'program' ID ';' { $2 }

VarDecBlock :: {[VarDec]}
    : { [] } -- nothing; empty list
    | 'var' VarDecs { $2 }

VarDecs :: {[VarDec]}
    : VarDec ';' { [$1] }
    | VarDec ';' VarDecs { $1:$3 }

VarDec :: {VarDec}
    : ID_List ':' Type { VarDec $1 $3 }
    | ID ':' 'real' '=' float { VarDef_Float $1 $5 }
    | ID ':' 'boolean' '=' 'true' { VarDef_True $1 }
    | ID ':' 'boolean' '=' 'false' { VarDef_False $1 }

ID_List :: {[String]}
    : ID { [$1] }
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
    | Exp '-' Exp { Op2 "-" $1 $3 }
    | Exp '*' Exp { Op2 "*" $1 $3 }
    | Exp '/' Exp { Op2 "/" $1 $3 }
    | Exp 'mod' Exp { Op2 "mod" $1 $3 }
    | 'sin' '(' Exp ')' { Op3 "sin" $3 }
    | 'cos' '(' Exp ')' { Op3 "cos" $3 }
    | 'exp' '(' Exp ')' { Op3 "exp" $3 }
    | 'ln' '(' Exp ')' { Op3 "ln" $3 }
    | 'sqrt' '(' Exp ')' { Op3 "sqrt" $3 }
    | '(' Exp ')' { $2 } -- ignore brackets
    | float { Real $1 }
    | ID { Var_R $1 }

BoolExp :: {BoolExp}
    : 'true' { True_C }
    | 'false' { False_C }
    | 'not' BoolExp { Not $2 }
    | BoolExp 'and' BoolExp { OpB "and" $1 $3 }
    | BoolExp 'or' BoolExp { OpB "or" $1 $3 }
    | BoolExp '=' BoolExp { OpB "=" $1 $3 }
    | BoolExp '<>' BoolExp { OpB "<>" $1 $3 }
    | Exp '>' Exp { OpR ">" $1 $3 }
    | Exp '>=' Exp { OpR ">=" $1 $3 }
    | Exp '<' Exp { OpR "<" $1 $3 }
    | Exp '<=' Exp { OpR "<=" $1 $3 }
    | Exp '=' Exp { OpR "=" $1 $3 }
    | Exp '<>' Exp { OpR "<>" $1 $3 }
    | '(' BoolExp ')' { $2 } -- ignore brackets
    | ID { Var_B $1 }

Statements :: {[Statement]}
    : { [] } -- nothing; make empty list
    | Statement { [$1] }
    | Statement ';' Statements { $1:$3 } -- add statement to list

Statement :: {Statement}
    : ID ':=' Exp { AssignR $1 $3 }
    | ID ':=' BoolExp { AssignB $1 $3 }
    | 'if' BoolExp 'then' Statement { IfThen $2 $4 }
    | 'if' BoolExp 'then' Statement 'else' Statement { IfThenElse $2 $4 $6 }
    | 'while' BoolExp 'do' 'begin' Statements 'end' { WhileDo $2 $5 }
    | 'for' ID ':=' float 'to' float 'do' 'begin' Statements 'end' { ForDo $2 $4 $6 $9 }
    | 'case' Exp 'of' CaseLabels 'end' { Case $2 $4 }
    | 'function' ID VarDec 'begin' Statement 'end' { Func "function" $2 [$3] "begin" $5 "end" }
    | 'procedure' ID VarDec 'begin' Statement 'end' { Proc "procedure" $2 [$3] "begin" $5 "end" }

CaseLabels :: {[CaseLabel]}
    : CaseLabel ';' { [$1] }
    | CaseLabel ';' CaseLabels { $1:$3 }

CaseLabel :: {CaseLabel}
    : Exp ':' Statement { CaseLabel $1 $3 }

{}
