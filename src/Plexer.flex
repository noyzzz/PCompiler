import java.io.*;
import java_cup.runtime.*;


%%
%{
      private Symbol symbol(String name, int sym) {
          return symbolFactory.newSymbol(name, sym, new Location(yyline+1,yycolumn+1,yychar), new Location(yyline+1,yycolumn+yylength(),yychar+yylength()));
      }

      private Symbol symbol(String name, int sym, Object val) {
          Location left = new Location(yyline+1,yycolumn+1,yychar);
          Location right= new Location(yyline+1,yycolumn+yylength(), yychar+yylength());
          return symbolFactory.newSymbol(name, sym, left, right,val);
      }
%}

%cup
%public
%class Plexer
%type Symbol
%line
%column
Identifier = [_a-zA-Z][_a-zA-Z0-9]*
DecIntegerLiteral = [0-9]+
BoolLiteral = true | false
new_line = \r|\n|\r\n;
white_space = {new_line} | [ \t\f]
BigComment = "/#" ~"#/"
HexNumber = 0[xX][0-9a-fA-F]+
ScientificNumber = {DecIntegerLiteral}+[eE]-?{DecIntegerLiteral}
LineComment = "##" ~\r|"//" ~\n|"//" ~\r\n
NormalCharacter = \'[^\\]\'
SpecialCharacter = \'[\\][^]\'
%state STRING

%%

<YYINITIAL>{
/* keywords */
{LineComment}          {/**/}
{BigComment}           {/**/}
/* names */
{Identifier}           { return symbol("Identifier",ID, yytext()); }

/* bool literal */
{BoolLiteral} { return symbol("boolconst",BOOL_CONST, new Boolean(Boolean.parseBool(yytext()))); }

/* literals */
{DecIntegerLiteral} { return symbol("intconst",INT_CONST, new Integer(Integer.parseInt(yytext()))); }
{NormalCharacter}                     {return symbol("character",CHAR_CONST,new Character(yytext().charAt(1)));}
{SpecialCharacter}                     {return symbol("character",CHAR_CONST,new Character(yytext()));}
    /* Keywords */
    "begin"                         {return symbol("begin",BEGIN);}
    "bool"                          {return symbol("bool",BOOL);}
    "break"                         {return symbol("break",BREAK);}
    "case"                          {return symbol("case",CASE);}
    "char"                          {return symbol("char",CHAR);}
    "const"                         {return symbol("const",CONST);}
    "continue"                      {return symbol("continue",CONTINUE);}
    "default"                       {return symbol("default",DEFAULT);}
    "double"                        {return symbol("double",DOUBLE);}
    "else"                          {return symbol("else",ELSE);}
    "end"                           {return symbol("end",END);}
    "extern"                        {return symbol("extern",EXTERN);}
    "false"                         {return symbol("false",FALSE);}
    "function"                      {return symbol("function",FUNCTION);}
    "float"                         {return symbol("float",FLOAT);}
    "for"                           {return symbol("for",FOR);}
    "goto"                          {return symbol("goto",GOTO);}
    "if"                            {return symbol("if",IF);}
    "input"                         {return symbol("input",INPUT);}
    "int"                           {return symbol("int",INT, new Integer( INTTYPE ));}
    "long"                          {return symbol("long",LONG);}
    "output"                        {return symbol("output",OUTPUT);}
    "return"                        {return symbol("return",RETURN);}
    "record"                        {return symbol("record",RECORD);}
    "sizeof"                        {return symbol("sizeof",SIZEOF);}
    "static"                        {return symbol("static",STATIC);}
    "string"                        {return symbol("string",STRING);}
    "switch"                        {return symbol("switch",SWITCH);}
    "true"                          {return symbol("true",TRUE);}
    "repeat"                          {return symbol("repeat",REPEAT);}



    /* Seprators & Operators */
    "=="                            {return symbol("isequal", ISEQUAL);}
    "!="                            {return symbol("notEqual", NOTEQUAL);}
    "<="                            {return symbol("leq", LEQ);}
    "<"                             {return symbol("less", LESS);}
    ">"                             {return symbol("greater", GREATER);}
    ">="                            {return symbol("geq", GEQ);}
    "="                             {return symbol("equal", EQUAL);}
    "not"                           {return symbol("not", NOT);}
    "~"                             {return symbol("tilde", TILDE);}
    "&"                             {return symbol("ampersand", AMPERSAND);}
    "and"                           {return symbol("and", AND);}
    "|"                             {return symbol("pipe", PIPE);}
    "or"                            {return symbol("or", OR);}
    "^"                             {return symbol("caret", CARET);}
    "*"                             {return symbol("times", TIMES);}
    "+"                             {return symbol("plus", PLUS);}
    "/"                             {return symbol("slash", SLASH);}
    "%"                             {return symbol("mod", MOD);}
    "{"                             {return symbol("lbrace", LBRACE);}
    "}"                             {return symbol("rbrace", RBRACE);}
    "("                             {return symbol("lpar", LPAR);}
    ")"                             {return symbol("rpar", RPAR);}
    ","                             {return symbol("comma", COMMA);}
    ":"                             {return symbol("colon", COLON);}
    ";"                             {return symbol("semicolon", SEMICOLON);}
    "["                             {return symbol("openingBrace", OPENINGBRACE);}
    "]"                             {return symbol("closingBrace", CLOSINGBRACE);}


{white_space}     { /* ignore */ }

}

<STRING> {
  \"                             { yybegin(YYINITIAL);
      return symbol("StringConst",STRINGCONST,string.toString(),string.length()); }
  [^\n\r\"\\]+                   { string.append( yytext() ); }
  \\t                            { string.append('\t'); }
  \\n                            { string.append('\n'); }

  \\r                            { string.append('\r'); }
  \\\"                           { string.append('\"'); }
  \\                             { string.append('\\'); }
}