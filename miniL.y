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
#include <string>
#include <stack>
#include <vector>
#include <map>

using namespace std;

enum symbolType {INT, INTARR, FUNC};

void yyerror(string msg);

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
extern FILE *yyin;

string makeTemp() {
  stringstream ss;
  ss << tempCount++;
  string temp = "__temp" + ss.str();
  return temp;
}

string makeLabel() {
  stringstream ss;
  ss << labelCount++;
  string temp = "__label" + ss.str();
  return temp;
}

void yyerror(const char * msg) {
    /* implement your error handling */
    printf("Error: On line %d, column %d: %s \n", currentLine, currentPosition, msg);
}

void yyerror(string msg) {
   cout << "Line : " << currentLine << " Coloumn : " << currentPosition << " " << msg << endl;
}

void addFunct(Funct f) {
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


%left MOD 
%left EQ
%left NEQ 
%left LT
%left GT
%left LTE
%left GTE

/* comparison operators */

%left ADD
%left SUB 
%left MULT
%left DIV

%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET
%token L_PAREN
%token R_PAREN

%token ASSIGN
%token NOT

/* identifiers and numbers */
%token <numVal> NUM
%token <identVal> IDENT

/* other special symbols */
%token SEMICOLON
%token COLON
%token COMMA


/* grammars */
%type <attr> var
%type <attr> expression
%type <attr> term
%type <attr> declarations declaration 
%type <attr> statement
%type <attr> mul_exp
%type <attr> bool_exp
%type <attr> expression_list
%type <identVal> comp
%type <attr> vars dos ifs whiles reads writes continues breaks returns
%type <attr> nothing

/* %start program */
%start start



    /* write your rules here */

%%

start : /*epsilon*/
	|start function;


function : FUNCTION IDENT {inCode << "func " << string($2) << endl;} SEMICOLON BEGIN_PARAMS 
          declarations { 
            paramCount = 0;
            while (!paramStack.empty()){
              inCode << "= " << paramStack.top() << ", " << "$" << paramCount++ << endl;
              paramStack.pop();
            }
          } 
          END_PARAMS  BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY {
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
		cout << "mainExists : " << mainExists << endl;
          };

declarations: /*epsilon*/
		|declaration SEMICOLON declarations
		|declaration 
		;

declaration:	IDENT COLON INTEGER {
	    
               identStack.push($1);
               paramStack.push($1);
               while(!identStack.empty()) {
                 string out = identStack.top();
                 Symbol sym(0, 0, out, INT); 
                 addSymbol(sym);
                 inCode << ". " << out << endl;
                 identStack.pop(); 
               }
             }
		|IDENT COLON ARRAY L_SQUARE_BRACKET NUM R_SQUARE_BRACKET OF INTEGER {
               identStack.push($1);
               paramStack.push($1);
               while(!identStack.empty()) {
                 string out = identStack.top();
                 Symbol sym(0, $5, out, INTARR);
                 addSymbol(sym);
                 if($5 == 0){
                   yyerror("Error: Array cannot be size 0.");
                 }
                 inCode << ".[] " << out << ", " << $5 << endl;
                 identStack.pop(); 
               }
             };
	
statements:	statement SEMICOLON statements
		|statement SEMICOLON
		|statement 
		;

statement:	vars
	  	|ifs
		|whiles
		|dos
		|reads
		|writes
		|continues
		|breaks
		|returns
		;

vars:      var ASSIGN expression {
		if($1.type == INTARR){
		inCode << "[]= " << const_cast<char*>($1.name) << ", " << const_cast<char*>($1.ind) << ", " << const_cast<char*>($3.name) << endl;
		}
		else {
	      	inCode << "= " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
		}
	      outCode << inCode.rdbuf();
	      inCode.clear();
 	      inCode.str(" ");	
           };


ifs:	IF bool_exp THEN statements ENDIF {
             string start = makeLabel();
             string endif = makeLabel();
             labelStack.push(endif); 
             inCode << "?:= " << start << ", " << const_cast<char*>($2.name) << endl;
             inCode << ":= " << endif << endl;
             inCode << ": " << start << endl;
           }
 
        |IF bool_exp THEN statements ELSE statements ENDIF {
             inCode << ": " << labelStack.top() << endl;
             labelStack.pop();
             
             outCode << inCode.rdbuf();
             inCode.clear();
             inCode.str(" ");

           };

whiles:	WHILE bool_exp BEGINLOOP {
              string condition = makeLabel();
              string endlabel = makeLabel();
              string start = makeLabel();
              outCode << ": " << start << endl;
              outCode << inCode.rdbuf();
              inCode.clear();
              inCode.str(" ");
              inCode << "?:= " << condition << ", " << const_cast<char*>($2.name) << endl;
              inCode << ":= " << endlabel << endl;
              inCode << ": " << condition << endl;
              labelStack.push(start);
              labelStack.push(endlabel);

            } statements ENDLOOP {
                outCode << inCode.rdbuf();
                inCode.clear();
                inCode.str(" ");
                string endlabel = labelStack.top();
                labelStack.pop();
                string start = labelStack.top();
                labelStack.pop();
                inCode << ":= " << start << endl;
                inCode << ": " << endlabel << endl;
                outCode << inCode.rdbuf();
                inCode.clear();
                inCode.str(" ");
           };

dos:	DO BEGINLOOP {
             string start = makeLabel();
             labelStack.push(start);
             outCode << ": " << start << endl;
             outCode << inCode.rdbuf();
             inCode.clear();
             inCode.str(" ");
            }
           statements ENDLOOP WHILE bool_exp {
             string start = labelStack.top();
             inCode << "?:= " << start << ", " << const_cast<char*>($7.name) << endl;
             labelStack.pop(); 
             outCode << inCode.rdbuf();
             inCode.clear();
             inCode.str(" ");
           };

var_loop : /*epsilon*/
	   | COMMA var var_loop
  		{varStack.push($2.name);}
	   ;

reads:  READ var var_loop {
             varStack.push($2.name);
             while (!varStack.empty()) {
                if ($2.type == 0) {
                    inCode << ".< " << varStack.top() << endl;
                    varStack.pop();
                }
                else {
                    inCode << ".[]< " << varStack.top() << ", "  <<  const_cast<char*>($2.ind) << endl;
                    varStack.pop();
                }
             }
             outCode << inCode.rdbuf();
             inCode.clear();
             inCode.str(" ");
          };
     
writes: WRITE var var_loop {
	    varStack.push($2.name);
            while (!varStack.empty()) {
                if ($2.type == 0) {
                    inCode << ".> " << varStack.top() << endl;
                    varStack.pop();
                }
                else {
		    string out = makeTemp();
	            strcpy($$.name, out.c_str());
		    inCode << ". " << out << endl;
		    inCode << "=[] " << out << ", " << const_cast<char*>($2.name) << ", " << const_cast<char*>($2.ind) << endl;
                    inCode << ".[]> " << varStack.top() << ", "  <<  const_cast<char*>($2.ind) << endl;
                    varStack.pop();
                }
            }
            outCode << inCode.rdbuf();
            inCode.clear();
            inCode.str(" ");
         };

continues:  CONTINUE {
             if (!labelStack.empty()) {
               inCode << ":= " << labelStack.top() << endl;
               outCode << inCode.rdbuf();
               inCode.clear();
               inCode.str(" ");
             }
             else {
               yyerror("Error: Continue used outside of loop");
             }
           };

breaks : BREAK {
             if (!labelStack.empty()) {
               inCode << ":= " << labelStack.top() << endl;
               outCode << inCode.rdbuf();
               inCode.clear();
               inCode.str(" ");
             }
             else {
               yyerror("Error: Break used outside of loop");
             }
           };


returns:    RETURN expression {
             $$.val = $2.val;
             strcpy($$.name, $2.name);
             inCode << "ret " << const_cast<char*>($2.name) << endl;
             outCode << inCode.rdbuf();
             inCode.clear();
             inCode.str(" ");
         };

bool_exp : bool_exp expression comp expression {
             string out = makeTemp();
             strcpy($$.name, out.c_str());
             inCode << ". " << out << endl;
           }
	   |NOT bool_exp {
             string out = makeTemp();
             strcpy($$.name, out.c_str());
             inCode << ". " << out << endl;
             inCode << "! " << out << ", " << const_cast<char*>($2.name) << endl;
           }
	   |/*epsilon*/
	   ;

comp: EQ { $$ = const_cast<char*>("=="); } 
    | NEQ { $$ = const_cast<char*>("!="); }
    | LT { $$ = const_cast<char*>("<"); }
    | GT { $$ = const_cast<char*>(">"); }
    | LTE { $$ = const_cast<char*>("<="); }
    | GTE { $$ = const_cast<char*>(">="); };

expression : mul_exp {
              strcpy($$.name,$1.name);
	      $$.type = $1.type;
             }
		|expression ADD mul_exp {
              cout << "plus";
	      string out = makeTemp();
	      inCode << ". " << out << endl;
              inCode << "+ " << out << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
              strcpy($$.name, out.c_str());
		
            }
		| expression SUB mul_exp {
	      cout << "minus";
	      string out = makeTemp();
              inCode << ". " << out << endl;
              inCode << "- " << out << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
              strcpy($$.name, out.c_str());
            }
		;

mul_exp : term {
		strcpy($$.name, $1.name);
		$$.type = $1.type;
		}
       	|mul_exp MULT term {
	string out = makeTemp();
	inCode << ". " << out << endl;
        inCode << "* " << out << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
        strcpy($$.name, out.c_str());
		}
        |mul_exp DIV term {
        string out = makeTemp();
        inCode << ". " << out << endl;
        inCode << "/ " << out << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
        strcpy($$.name, out.c_str());
		}
        |mul_exp MOD term {
        string out = makeTemp();
        inCode << ". " << out << endl;
        inCode << "% " << out << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
        strcpy($$.name, out.c_str());
	};


term :  var {
        $$.val = $1.val;
        $$.type = $1.type;
        if ($1.type != 1) {
          strcpy($$.name, $1.name);
          strcpy($$.ind, $$.name);
	  cout << "term -> var :, var = " << $1.name << endl;
          /*inCode << ". " << const_cast<char*>($$.name) << endl;*/
          /*inCode << "= " << const_cast<char*>($$.name) <<  ", " << const_cast<char*>($1.name) << endl;*/
        }
        else if ($1.type == 1) {
          strcpy($$.name, makeTemp().c_str());
	  inCode << ". " <<  const_cast<char*>($$.name) << endl;
          inCode << "=[] " << const_cast<char*>($$.name) << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($1.ind) << endl;
        }

      }
    | NUM {
        $$.val = $1;
        $$.type = 0;
	char num_char[100];
	sprintf(num_char, "%d", $1);	
        strcpy($$.name, num_char);
        strcpy($$.ind, $$.name);
        /*inCode << ". " << const_cast<char*>($$.name) << endl;
        inCode << "= " << const_cast<char*>($$.name) <<  ", " << $$.val << endl;*/
      }
     | L_PAREN expression R_PAREN {
       strcpy($$.name, $2.name);
       
      }
    
     | IDENT L_PAREN expression_list R_PAREN {
        checkFunct(const_cast<char*>($1));
        expStack.push($3.name); 
        while (!expStack.empty()){
          inCode << "param " << expStack.top() << endl;
          expStack.pop();
        }
        string out = makeTemp();
        inCode << ". " << out << endl;
        inCode << "call " << const_cast<char*>($1) << ", " << out << endl;
        strcpy($$.name, out.c_str());
      }
    | IDENT L_PAREN R_PAREN {
        checkFunct(const_cast<char*>($1));
        string out = makeTemp();
        inCode << ". " << out << endl;
        inCode << "call " << const_cast<char*>($1) << ", " << out << endl;
        strcpy($$.name, out.c_str());
      };

expression_list : expression 
		| expression_list COMMA expression
		{expStack.push($3.name);}
		;
nothing : /*epsilon*/

var : IDENT {

	checkSymbol($1);
        if(symbolTable[$1].type == INTARR) {
        yyerror("Symbol is of type int array");
       }
       else {
	cout << "$1 = " << $1 << endl;
	strcpy($$.name , $1);	
	$$.type = 0;
 	}
	
     }
   
      |IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET {
       checkSymbol($1);
       if(symbolTable[$1].type == INT) {
         yyerror("Symbol is of type int");
       }
       else {
          if ($3.type == 1) {
           string out = makeTemp();
           $$.type = 1;
           strcpy($$.ind, out.c_str());
           strcpy($$.name, $1);
           inCode << ". " << out << endl; 
           inCode << "=[] " << out << ", " << const_cast<char*>($3.name) << ", " << const_cast<char*>($3.ind) << endl;
         }
         else {
           strcpy($$.name, $1);
           $$.type = 1;
           strcpy($$.ind, $3.name);
         }
       }
     };

%%



int main(int argc, char **argv) {
   if (argc > 1) {
    yyin = fopen(argv[1], "r");
    if (yyin == NULL) {
      printf("Syntax: %s filename\n", argv[0]);
    }
  }

  yyparse();

  if(mainExists==0){
    yyerror("Error: Main function not found.");
  }

  ofstream file;
  file.open("output.mil");
  file << outCode.str();
  file.close();
  return 0;
}
