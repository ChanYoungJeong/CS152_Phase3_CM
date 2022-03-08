   /* cs152-miniL */

%{   
   /* write your C code here for definitions of variables and including headers */
   /*#include "y.tab.h"*/
   #include "miniL-parser.hpp"
   int currentLine = 1;
   int currentPosition = 0; 
%}

/* some common rules */
SPACE    [\f | \r | \s | \t]
NUM      [0-9]+
ALPHANUM [A-Za-z][A-Za-z0-9]*([A-Za-z0-9]*|[_][_]*[A-Za-z0-9][A-Za-z0-9]*)+
BOTH     [A-Za-z0-9][A-Za-z0-9_]*[_]
IDENT    [0-9_][A-Za-z0-9_]*[A-Za-z0-9_]
SUB      [-]
ADD      [+]
MULT     [*]
DIV      [/]
MOD      [%]
EQ       [=][=]
NEQ      [<][>]
LT       [<]
GT       [>]
LTE      [<][=]
GTE      [>][=]
SEMICOLON [;]
COLON     [:]
COMMA     [,]
L_PAREN   [(]
R_PAREN   [)]
/* L_SQUARE_BRACKET ['\['] */
/* R_SQUARE_BRACKET ['\]'] */
ASSIGN    [:][=]
LINE      [\n]


%%
   /* specific lexer rules in regex */

"function"           	 {currentPosition += 8; return FUNCTION;}
"beginparams"            {currentPosition += 11; return BEGIN_PARAMS;}
"endparams"          	 {currentPosition += 10; return END_PARAMS;} 
"beginlocals"            {currentPosition += 11; return BEGIN_LOCALS;}
"endlocals"    	         {currentPosition += 9; return END_LOCALS;}
"beginbody"              {currentPosition += 9; return BEGIN_BODY;}
"endbody"                {currentPosition += 7; return END_BODY;}
"integer"           	 {currentPosition += 7; return INTEGER;}
"array"            	 {currentPosition += 5; return ARRAY;}
"of"            	 {currentPosition += 2; return OF;}
"if"                     {currentPosition += 2; return IF;}
"then"                   {currentPosition += 4; return THEN;}
"endif"                  {currentPosition += 5; return ENDIF;}
"else"                   {currentPosition += 4; return ELSE;}
"while"                  {currentPosition += 5; return WHILE;}
"do"                     {currentPosition += 2; return DO;}
"beginloop"              {currentPosition += 9; return BEGINLOOP;}
"endloop"                {currentPosition += 7; return ENDLOOP;}
"continue"               {currentPosition += 8; return CONTINUE;}
"break"                  {currentPosition += 5; return BREAK;}
"read"                   {currentPosition += 4; return READ;}
"write"                  {currentPosition += 5; return WRITE;}
"not"                    {currentPosition += 3; return NOT;}
"true"                   {currentPosition += 4; return TRUE;}
"false"                  {currentPosition += 5; return FALSE;}
"return"                 {currentPosition += 6; return RETURN;}

"-"         		 {currentPosition += 1; return SUB;}
{ADD}                    {currentPosition += 1; return ADD;}
{MULT}                   {currentPosition += 1; return MULT;}
{MOD}                    {currentPosition += 1; return MOD;}
{DIV}                    {currentPosition += 1; return DIV;}
{EQ}                     {currentPosition += 2; return EQ;}
{NEQ}                    {currentPosition += 2; return NEQ;}
{LT}                     {currentPosition += 1; return LT;}
{GT}                     {currentPosition += 1; return GT;}
{LTE}                    {currentPosition += 2; return LTE;}
{GTE}                    {currentPosition += 2; return GTE;}

{SEMICOLON}              {currentPosition += 1; return SEMICOLON;}
{COLON}                  {currentPosition += 1; return COLON;}
{COMMA}                  {currentPosition += 1; return COMMA;}
{L_PAREN}                {currentPosition += 1; return L_PAREN;}
{R_PAREN}                {currentPosition += 1; return R_PAREN;}
"["                      {currentPosition += 1; return L_SQUARE_BRACKET;}
"]"                      {currentPosition += 1; return R_SQUARE_BRACKET;}
{ASSIGN}	         {currentPosition += 2; return ASSIGN;}


{SPACE}	  {currentPosition += 1;}
{LINE}	  {currentLine += 1; currentPosition = 0;}
{NUM}      {currentPosition += yyleng; yylval.numVal = atoi(yytext); return NUM;}
{ALPHANUM} {currentPosition += yyleng; char *ptr = new char[yyleng]; strcpy(ptr, yytext); yylval.identVal = ptr; return IDENT;}
{IDENT}    {printf("Error at line %d, column %d: Identifier \"%s\" must begin with a letter\n", currentLine, currentPosition, yytext); exit(0);}
{BOTH}     {printf("Error at line %d, column %d: Identifier \"%s\" cannot end with an underscore\n", currentLine, currentPosition, yytext); exit(0);}
##.*       {currentLine += 1; currentPosition = 0;}


.          {printf("Error at line %d, column %d: Identifier \"%s\" unrecognized symbol\n", currentLine, currentPosition, yytext); exit(0);}

%%
	/* C functions used in lexer */
