/* cs152-miniL phase3 */

%{
void yyerror(const char *msg);
extern int yylex();
extern int currentLine;
extern int currentPosition;

#include "lib.h"
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <sstream>
#include <fstream>
#include <string.h>
#include <stack>
#include <vector>
#include <map>
using namespace std;

enum symbolType {INT, INTARR, FUNC};

struct Symbol {
  int val;
  int size;
  string name;
  symbolType type; 
  Symbol(): val(0), size(0), name(), type() {}
  Symbol(int v, int s, string n, symbolType t): val(v), size(s), name(n), type(t) {}
};

struct Funct {
  string name;
  Funct(): name() {}
  Funct(string n) : name(n) {}
};

map<string, Symbol> symbolTable;
map<string, Funct> functTable;
stack<string> varStack; 
stack<string> identStack;
stack<string> expStack; 
stack<string> labelStack;
stack<string> paramStack;
stringstream inCode;
ostringstream outCode;
int tempCount = 0;
int labelCount = 0;
int paramCount = 0;
bool mainExists = 0;

string makeTemp() {
  stringstream ss;
  ss << tempCount++;
  string temp = "__temp__" + ss.str();
  return temp;
}

string makeLabel() {
  stringstream ss;
  ss << labelCount++;
  string temp = "__label__" + ss.str();
  return temp;
}

void yyerror(const char *msg) {
    /* implement your error handling */
    printf("Error: On line %d, column %d: %s \n", currentLine, currentPosition, msg);
}

void addFunc(Funct f) {
  if (functTable.find(f.name) == functTable.end()) {
    functTable[f.name] = f;
  }
  else {
    string error = "Function is already declared: " + f.name;
    yyerror(error);
  }
}

void addSymbol(Symbol s) {
  if (symbolTable.find(s.name) == symbolTable.end()) {
    symbolTable[s.name] = s;
  }
  else {
    string error = "Symbol is already declared: " + s.name;
    yyerror(error);
  }
}

void checkFunct(string name) {
  if(functTable.find(name) == functTable.end()) {
    string error = "Function is not declared: " + name;
    yyerror(error);
  }
}

void checkSymbol(string name) {
  if(symbolTable.find(name) == symbolTable.end()) {
    string error = "Symbol is not declared: " + name;
    yyerror(error);
  }
}

%}

%union{
  /* put your types here */
  char* identVal;
  int numVal;

  struct attributes {
    char name[100];
    char ind[100];
    int type;
    int val;
    int size;
  } attr;
}

%error-verbose
%locations

/* reserved words */
%token FUNCTION
%token BEGIN_PARAMS
%token END_PARAMS
%token BEGIN_LOCALS
%token END_LOCALS
%token BEGIN_BODY
%token END_BODY
%token INTEGER
%token ARRAY
%token OF
%token IF
%token THEN
%token ENDIF
%token ELSE
%token WHILE
%token DO
%token BEGINLOOP
%token ENDLOOP
%token CONTINUE
%token BREAK
%token READ
%token WRITE
%token TRUE
%token FALSE
%token RETURN

/* arithmetic operators */

/* comparison operators */
%token EQ
%token NEQ
%token LT
%token GT
%token LTE
%token GTE

%left ADD
%left SUB
%left MULT
%left DIV
%left MOD
%left L_SQUARE_BRACKET
%left R_SQUARE_BRACKET
%left L_PAREN
%left R_PAREN

%right ASSIGN
%right NOT

/* identifiers and numbers */
%token <numVal> NUM
%token <identVal> IDENT
%token <identVal> comp

/* other special symbols */
%token SEMICOLON
%token COLON
%token COMMA

/* grammars */
%type <attr> var
%type <attr> expression
%type <attr> term
%type <attr> declaration
%type <attr> statement
%type <attr> mul_exp
%type <attr> bool_exp
%type <attr> expression_list


/* %start program */
%start start



    /* write your rules here */

%%

start : /*epsilon*/{printf("start -> epsilon\n");}|
	start function 
	{printf("start -> start function");}
	;


function: FUNCTION IDENT {inCode << "func " << string($2) << endl;} SEMICOLON BEGIN_PARAMS 
          declarations { 
            paramCount = 0;
            while (!paramStack.empty()){
              inCode << "= " << paramStack.top() << ", " << "$" << paramCount++ << endl;
              paramStack.pop();
            }
          } 
          END_PARAMS  BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statement SEMICOLON statements END_BODY {
            outCode << "endfunc\n";
            symbolTable.clear();
            if (strcmp($2, "main")==0) {
              mainExists = 1;      
            }
            Funct f($2);
            addFunct(f);
            while (!paramStack.empty()) {
              paramStack.pop();
            }
          };

declarations: /*epsilon*/
	    {printf("declarations->epsilon\n");}
		|declaration SEMICOLON declarations
		{printf("declarations->declaration SEMICOLON declarations\n");}
		|declaration error {yyerror;}
		;

declaration:	identifiers COLON INTEGER
	   	{printf("declaration->identifiers COLON INTEGER\n");}
		|identifiers COLON ARRAY L_SQUARE_BRACKET NUM R_SQUARE_BRACKET OF INTEGER
		{printf("declaration->identifiers COLON ARRAY L_SQUARE_BRACKET NUM %d R_SQUARE_BRACKET OF INTEGER\n", $5);}
		;

	
statements:	statement SEMICOLON statements
	  	{printf("statements->statement SEMICOLON statements\n");}
		|statement SEMICOLON
		{printf("statements->statement SEMICOLON\n");}
		| statement error {yyerror;}
		;

statement:	vars
	  	{printf("statement->vars\n");}
	  	|ifs
		{printf("statement->ifs\n");}
		|whiles
		{printf("statement->whiles\n");}
		|dos
		{printf("statement->dos\n");}
		|reads
		{printf("statement->reads\n");}
		|writes
		{printf("statement->writes\n");}
		|continues
		{printf("statement->continues\n");}
		|breaks
		{printf("statement->breaks\n");}
		|returns
		{printf("statement->returns\n");}
		;

vars:  /*epsilon*/
         | COMMA var vars {
             varStack.push($2.name);
           };


ifs:	IF bool_exp THEN statements ENDIF
   		{printf("ifs->IF bool_exp THEN statements ENDIF\n");}
		|IF bool_exp THEN statements ELSE statements ENDIF
		{printf("ifs->IF bool_exp THEN statements ELSE statements ENDIF\n");}
		;

whiles:	WHILE bool_exp BEGINLOOP statements ENDLOOP
      	{printf("whiles->WHILE bool_exp BEGINLOOP statements ENDLOOP\n");}
		;

dos:	DO BEGINLOOP statements ENDLOOP WHILE bool_exp
   		{printf("dos-> DO BEGINLOOP statements ENDLOOP WHILE bool_exp\n");}
		;

varLoop:
       	{printf("varLoop->epsilon\n");}
		|COMMA var varLoop
		{printf("varLoop-> COMMA var varLoop\n");}
		;

reads:  READ var varLoop
     	{printf("reads->READ var varLoop\n");}
		;
     
writes: WRITE var varLoop
      	{printf("writes->WRITE var varLoop\n");}
		;

continues:  CONTINUE
	 	{printf("continues->CONTINUE\n");}
		;
breaks : BREAK
		{printf("breaks -> BREAK \n");}


returns:    RETURN expression
       	{printf("returns->RETURN expression\n");}
		;

bool_exp : bool_exp expression comp expression
	   {printf("bool_exp -> bool_exp expression comp expression\n");}
	   |NOT bool_exp
	   {printf("bool_exp -> NOT bool_exp\n");}
	   |/*epsilon*/
	   {printf("bool_exp -> epsilon\n");}
	   ;

comp: EQ { $$ = const_cast<char*>("=="); } 
    | NEQ { $$ = const_cast<char*>("!="); }
    | LT { $$ = const_cast<char*>("<"); }
    | GT { $$ = const_cast<char*>(">"); }
    | LTE { $$ = const_cast<char*>("<="); }
    | GTE { $$ = const_cast<char*>(">="); };

expression : mul_exp {printf("expression -> mul_exp\n");}
		| expression ADD mul_exp {printf("expression -> expression ADD mul_exp\n");}
		| expression SUB mul_exp {printf("expression -> expression SUB mul_exp\n");}
		;

mul_exp : term {
		 strcpy($$.name,$1.name);
         $$.type = $1.type;
		};
       	 |mul_exp MULT term {
		 string temp = make_temp();
         in_code << ". " << temp << endl;
         in_code << "* " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
         strcpy($$.name, temp.c_str());
		};
	 |mul_exp DIV term {
		 string temp = make_temp();
         inCode << ". " << temp << endl;
    	 inCode << "/ " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
         strcpy($$.name, temp.c_str());
		};
	 |mul_exp MOD term {
		 string temp = make_temp();
         inCode << ". " << temp << endl;
         inCode << "% " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
         strcpy($$.name, temp.c_str());
		};

term: SUB var {
        $$.val = $2.val*-1;
        $$.type = $2.type;
        if ($2.type != 1) {
          string zero = makeTemp();
          string num = makeTemp();
          inCode << ". " << zero << endl;
          inCode << "= " << zero << ", " << "0" << endl;
          inCode << ". " << num << endl;
          inCode << "= " << num << ", " << const_cast<char*>($2.name) << endl;
          strcpy($$.name, makeTemp().c_str());
          inCode << ". " << const_cast<char*>($$.name) << endl;
          inCode << "- " << const_cast<char*>($$.name) <<  ", " << zero << ", " << num << endl;
         }        
        else if ($2.type == 1) {
          string zero = makeTemp();
          string num = makeTemp();
          inCode << ". " << zero << endl;
          inCode << "= " << zero << ", " << "0" << endl;
          inCode << ". " << num << endl;
          inCode << ". " << num << endl;
          inCode << "=[] " << num << ", " << const_cast<char*>($2.name) <<  ", " << const_cast<char*>($2.index) << endl;
          strcpy($$.name, make_temp().c_str());
          inCode << ". " <<  const_cast<char*>($$.name) << endl;
          inCode << "- " << const_cast<char*>($$.name) << ", " << zero <<  ", " << num << endl;
        }
      }
    | var {
        $$.val = $1.val;
        $$.type = $1.type;
        if ($1.type != 1) {
          strcpy($$.name, makeTemp().c_str());
          strcpy($$.ind, $$.name);
          inCode << ". " << const_cast<char*>($$.name) << endl;
          inCode << "= " << const_cast<char*>($$.name) <<  ", " << const_cast<char*>($1.name) << endl;
        }
        else if ($1.type == 1) {
          strcpy($$.name, makeTemp().c_str());
          inCode << ". " <<  const_cast<char*>($$.name) << endl;
          inCode << "=[] " << const_cast<char*>($$.name) << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($1.ind) << endl;
        }

      }
    | SUB NUM {
        $$.val = $2*(-1);
        $$.type = 0;
        string zero = makeTemp();
        string num = makeTemp();
        inCode << ". " << zero << endl;
        inCode << "= " << zero << ", " << "0" << endl;
        inCode << ". " << num << endl;
        inCode << "= " << num << ", " << $2 << endl;

        strcpy($$.name, makeTemp().c_str());
        inCode << ". " << const_cast<char*>($$.name) << endl;
        inCode << "- " << const_cast<char*>($$.name) <<  ", " << zero << ", "<< num << endl;
     }
    | NUM {
        $$.val = $1;
        $$.type = 0;

        strcpy($$.name, makeTemp().c_str());
        strcpy($$.ind, $$.name);
        inCode << ". " << const_cast<char*>($$.name) << endl;
        inCode << "= " << const_cast<char*>($$.name) <<  ", " << $$.val << endl;
      }
     | SUB L_PAREN expression R_PAREN {

       string zero = makeTemp();

       inCode << ". " << zero << endl;
       inCode << "= " << zero << ", " << "0"<< endl;
        
       strcpy($$.name, make_temp().c_str());
       inCode << ". " << const_cast<char*>($$.name) << endl;
       inCode << "- " << const_cast<char*>($$.name) <<  ", " << zero << ", "<< const_cast<char*>($3.name) << endl;
      }
    | L_PAREN expression R_PAREN {
        strcpy($$.name, $2.name);
    }
    | IDENT L_PAREN expression expression_list R_PAREN {
        checkFunct(const_cast<char*>($1));
        expStack.push($3.name); 
        while (!expStack.empty()){
          inCode << "param " << expStack.top() << endl;
          expStack.pop();
        }
        string temp = make_temp();
        inCode << ". " << temp << endl;
        inCode << "call " << const_cast<char*>($1) << ", " << temp << endl;
        strcpy($$.name, temp.c_str());
      }
    | IDENT L_PAREN R_PAREN {
        checkFunct(const_cast<char*>($1));
        string temp = makeTemp();
        inCode << ". " << temp << endl;
        inCode << "call " << const_cast<char*>($1) << ", " << temp << endl;
        strcpy($$.name, temp.c_str());
      };

expression_list : /*epsilon*/
		| COMMA expression expression_list
		{expStack.push($2.name);}
		;

var: IDENT {
       checkSymbol($1);
       if(symbolTable[$1].type == INTARR) {
         yyerror("Symbol is of type int array");
       }
       else {
         strcpy($$.name,$1);
         $$.type = 0;
       }
     }
   | IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET {
       checkSymbol($1);
       if(symbolTable[$1].type == INT) {
         yyerror("Symbol is of type int");
       }
       else {
         if ($3.type == 1) {
           string temp = makeTemp();
           $$.type = 1;
           strcpy($$.index, temp.c_str());
           strcpy($$.name, $1);
           inCode << ". " << temp << endl; 
           inCode << "=[] " << temp << ", " << const_cast<char*>($3.name) << ", " << const_cast<char*>($3.index) << endl;
         }
         else {
           strcpy($$.name, $1);
           $$.type = 1;
           strcpy($$.index, $3.name);
         }
       }
     };

numbers: NUM
		{printf("numbers -> NUM %d\n", $1);}
	;

identifiers: IDENT
		{printf("identifiers->IDENT %s\n", $1);}

%%



int main(int argc, char **argv) {
   yyparse();
   return 0;
}
