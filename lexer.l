%{
    #include <stdlib.h>

    #include "y.tab.h"
    #include "symbols.h"
    int cInteger;
    char* vName;
    //extern symList* myVars;
    //int constlist = 0;
%}

%option noyywrap
%%

[ \t]+                          {;}

"return"                        {return RETURN;}
"TypeOf"                        {return TYPEOF;}
"eval"                          {return EVAL;}
"methods:"                       {return METHODS;}
"classes:"                       {return CDECL;}
"functions:"                     {return FDECL;}
"class"                         {return CLASS; }
"const"                         {return CONST;}
"int"                           {return INT;}
"float"                         {return FLOAT;}
"char"                          {return CHAR;}
"bool"                          {return BOOL;}
"string"                        {return STRING; }

"main"                          {return MAIN_FUN; }

%{
    // operatori booleeni
%}
"/\\"                           {return AND; }
"\\/"                           {return OR; }
"=="                            {return EQ; }
"!="                            {return NEQ; }
"<="                            {return LEQ; }
">="                            {return GEQ; }

"++"                            {return APPEND; }
"if"                            {return IF; }
"else"                          {return ELSE; }
"while"                         {return WHILE; }
"for"                           {return FOR;}
%{
    /* constante */
%}
"true"|"false"                  {
                                    yylval.bval = !strcmp(yytext, "true") ? 1 : 0;
                                    return BOOLEAN; }
[+-]?([0-9]|[1-9][0-9]+)        {
                                    cInteger = atoi(yytext);
                                    yylval.ival = cInteger;
                                    return INTEGER; }
'.'                             {   
                                    yylval.ch = yytext[1];
                                    return CHARACTER; }

([0-9]+|([0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?) {
                                    yylval.fval = atof(yytext);
                                    return CFLOAT; }
\"[^"\n]*\"                     {   
                                    yylval.str = strdup(yytext + 1);
                                    yylval.str[strlen(yylval.str) - 1] = '\0';
                                    return CSTRING; }

[a-zA-Z][_a-zA-Z0-9]*            {
                                    yylval.str = strdup(yytext);
                                    return ID; }



\n                              {yylineno++; }
.                               {return yytext[0]; }

%%