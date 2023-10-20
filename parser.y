%{
    #define ENABLE_PRINTS 0
    #define ENABLE_PRINT_OPERATIONS 0
    #define allocMeC(s, t) ((t*)calloc(s, sizeof(t)))

    #include "functions.c"
    
    extern int yylex();
%}

%union{
    int ival;
    float fval;
    int bval;
    char ch;
    char* str;
    void* gh;
    struct symVar* var;
    struct symFun* fun;
    struct AST* arb;
    struct paramType* callList;
}

// IMPORTANT REZOLVAT ASSIGNEMENTURI
// apeluri de functii
// de verificat declararea claselor
// sintaxa specifica claselor
// return functii
// tipul datei returnate sa coincida cu tipul functiei/metodei

%token IF ELSE WHILE FOR FDECL CDECL
%token <str> ID
%token CONST INT BOOL STRING CHAR FLOAT
%token <bval>BOOLEAN <ival>INTEGER <fval>CFLOAT <str>CSTRING <ch>CHARACTER
%token CLASS  METHODS RETURN
%token MAIN_FUN EVAL TYPEOF
%token AND OR EQ NEQ LEQ GEQ APPEND

//%type <var> lista_variabile
%type <fun> nume
%type <var> lista_param 
%type <callList> lista_apel lista_apel_expr
%type <fun> antet
%type <arb> constants expression
%type <var> ids

%left OR
%left AND
%nonassoc NOT
%left LEQ '<' GEQ '>' EQ NEQ
%left APPEND
%left '+' '-'
%left '*' '/' '%'
%nonassoc UNNOT

%start program
%%

program : algoritm {printf("Program corect sintactic!\n"); }
        ;

algoritm : declaratii_variabile FDECL {currScope = _function;} declaratii_functii 
           CDECL {currScope = _class;} declaratii_clase {currScope = _main; bzero(currClass, 32);}
           main
         ;

type : sub_type main_type
     ;

sub_type :              {Const = 0;}
         | CONST        {Const = 1;}
         ;

main_type : INT         {Type = _int;}
          | FLOAT       {Type = _float;}
          | CHAR        {Type = _char;}
          | STRING      {Type = _string;}
          | BOOL        {Type = _bool;}
          ;


declaratii_variabile : 
                     | declaratii_variabile declaratie_v
                     ;

declaratie_v : type lista_variabile ';'
             ;

lista_variabile : lista_variabile ',' ID '=' expression
                    {
                        {
                        if(Type != $5->type){
                            sprintf(errorBuffer, "Variable %s has type %i, while the value of expression has type %i!\n", $3, Type, $5->type);
                            yyerror(errorBuffer);
                        }
                        struct symVar* sp = (struct symVar*)malloc(sizeof(struct symVar));
                        sp->isConst = Const;
                        sp->name = strdup($3);
                        sp->type = Type;
                        sp->isVec = 0;
                        sp->nextVar = 0;
                        sp->value = copyValue($5->value, Type);
                        sp->line = yylineno;
                        // if(currScope != _class && currScope != _method){
                        //     sp->name = strdup($1);
                        // }else{
                        //     sp->name = constructMethodName(currClass, $1);
                        // }
                        sp->scope = strdup(gScope);
                        symlook(sp);
                        free($3);
                    }
                    }
                | lista_variabile ',' ID '[' INTEGER ']'
                    {
                        if(Const){
                            yyerror("A constant variable must be initialized!");
                        }
                        struct symVar* sp = (struct symVar*)malloc(sizeof(struct symVar));
                        sp->isConst = Const;
                        sp->type = Type;
                        sp->name = strdup($3);
                        sp->isVec = $5;
                        sp->nextVar = 0;
                        sp->line = yylineno;
                        switch(stVar->type){
                            case _bool: stVar->value = calloc($5, sizeof(int)); break;
                            case _int: stVar->value = calloc($5, sizeof(int)); break;
                            case _float: stVar->value = calloc($5, sizeof(float)); break;
                            case _char: stVar->value = calloc($5, sizeof(char)); break;
                            case _string: stVar->value = calloc($5, sizeof(char*));} 
                        if(currScope != _class && currScope != _method){
                            sp->name = strdup($3);
                        }else{
                            sp->name = constructMethodName(currClass, $3);
                        }
                        sp->scope = strdup(gScope);
#if ENABLE_PRINTS == 3
                        printf("Var: %s %i %i %i %s\n", sp->name, sp->isVec, sp->type, sp->isConst, sp->scope);
#endif
                        symlook(sp);
                        free($3);
                    }
                | lista_variabile ',' ID
                    {
                        if(Const){
                            yyerror("A constant variable must be initialized!");
                        }
                        struct symVar* sp = (struct symVar*)malloc(sizeof(struct symVar));
                        sp->isConst = Const;
                        sp->name = strdup($3);
                        sp->type = Type;
                        sp->isVec = 0;
                        sp->nextVar = 0;
                        sp->line = yylineno;
                        if(currScope != _class && currScope != _method){
                            sp->name = strdup($3);
                        }else{
                            sp->name = constructMethodName(currClass, $3);
                        }
                        sp->scope = strdup(gScope);
#if ENABLE_PRINTS == 3
                        printf("Var: %s %i %i %i %s\n", sp->name, sp->isVec, sp->type, sp->isConst, sp->scope);
#endif
                        symlook(sp);
                        free($3);
                    }
                | ID '=' expression 
                    {
                        if(Type != $3->type){
                            sprintf(errorBuffer, "Variable %s has type %i, while the value of expression has type %i!\n", $1, Type, $3->type);
                            yyerror(errorBuffer);
                        }
                        struct symVar* sp = (struct symVar*)malloc(sizeof(struct symVar));
                        sp->isConst = Const;
                        sp->name = strdup($1);
                        sp->type = Type;
                        sp->isVec = 0;
                        sp->nextVar = 0;
                        sp->value = copyValue($3->value, Type);
                        sp->line = yylineno;
                        // if(currScope != _class && currScope != _method){
                        //     sp->name = strdup($1);
                        // }else{
                        //     sp->name = constructMethodName(currClass, $1);
                        // }
                        sp->scope = strdup(gScope);
                        symlook(sp);
                        free($3);
                    }
                | ID '[' INTEGER ']'
                    {
                        if(Const){
                            yyerror("A constant variable must be initialized!");
                        }
                        struct symVar* sp = (struct symVar*)malloc(sizeof(struct symVar));
                        sp->type = Type;
                        sp->name = strdup($1);
                        sp->isConst = Const;
                        sp->isVec = $3;
                        sp->line = yylineno;
                        sp->nextVar = 0;
                        sp->scope = strdup(gScope);
                        switch(stVar->type){
                            case _bool: stVar->value = calloc($3, sizeof(int)); break;
                            case _int: stVar->value = calloc($3, sizeof(int)); break;
                            case _float: stVar->value = calloc($3, sizeof(float)); break;
                            case _char: stVar->value = calloc($3, sizeof(char)); break;
                            case _string: stVar->value = calloc($3, sizeof(char*));} 
                        if(currScope != _class && currScope != _method){
                            sp->name = strdup($1);
                        }else{
                            sp->name = constructMethodName(currClass, $1);
                        }
#if ENABLE_PRINTS == 3
                        printf("Var: %s %i %i %i %s\n", sp->name, sp->isVec, sp->type, sp->isConst, sp->scope);
#endif
                        symlook(sp);
                        free($1);
                    }
                | ID    
                    {
                        if(Const){
                            yyerror("A constant variable must be initialized!");
                        }
                        struct symVar* sp = (struct symVar*)malloc(sizeof(struct symVar));
                        sp->isConst = Const;
                        sp->name = strdup($1);
                        sp->type = Type;
                        sp->isVec = 0;
                        sp->nextVar = 0;
                        sp->line = yylineno;
                        sp->scope = strdup(gScope);
                        if(currScope != _class && currScope != _method){
                            sp->name = strdup($1);
                        }else{
                            sp->name = constructMethodName(currClass, $1);
                        }
#if ENABLE_PRINTS == 3
                        printf("Var: %s %i %i %i %s\n", sp->name, sp->isVec, sp->type, sp->isConst, sp->scope);
#endif
                        symlook(sp);
                        free($1);
                    }
                ;

declaratii_functii : /* epsilon */
                   | lista_functii
                   ;

lista_functii : antet '{' statements '}' {gScope[strlen(gScope) - strlen($1->name)] = '\0';} 
              | lista_functii antet '{' statements '}' {gScope[strlen(gScope) - strlen($2->name)] = '\0';}
              ;

antet : nume '(' lista_param ')'
        {
            $1->parameters = $3;
#if ENABLE_PRINTS == 1
            PrintMyFunc($1);
#endif
            funlook($1);
        }
      ;

nume : type ID
        {
            funcRet = Type;
            struct symFun* newFun = (struct symFun*)malloc(sizeof(struct symFun));
            newFun->line = yylineno;
            newFun->isConst = Const;
            newFun->type = Type;
            newFun->parameters = 0;
            newFun->nextFun = 0;
            // else
            if(!currClass[0]){
                newFun->name = strdup($2);
            }else{
                newFun->name = constructMethodName(currClass, $2);
            }
            bzero(gScope, strlen(gScope));
            strcpy(gScope, newFun->name);
            free($2);
            //printf("%s %i %i\n", newFun->name, newFun->isConst, newFun->type);
            $$ = newFun;
        }
     ;

lista_param : type ID 
                {
                    struct symVar* paramList = (struct symVar*)malloc(sizeof(struct symVar));
                    paramList->isConst = Const;
                    paramList->type = Type;
                    paramList->name = strdup($2);
                    paramList->nextVar = 0;
                    $$ = paramList;
                    free($2);
                }
            |   /* epsilon */
                {
                    $$ = 0;
                }
            |  lista_param ',' type ID
                {
                    struct symVar* paramList = (struct symVar*)malloc(sizeof(struct symVar));
                    paramList->isConst = Const;
                    paramList->type = Type;
                    paramList->name = strdup($4);
                    paramList->nextVar = $1;
                    $$=paramList;
                    free($4);
                }
            | type ID '[' INTEGER ']'
                {
                    struct symVar* paramList = (struct symVar*)malloc(sizeof(struct symVar));
                    paramList->isConst = Const;
                    paramList->type = Type;
                    paramList->name = strdup($2);
                    paramList->nextVar = 0;
                    paramList->isVec = cInteger;
                    $$=paramList;
                    free($2);
                }
            ;

declaratii_clase : 
                 | declaratii_clase CLASS ID {strcpy(gScope, $3); gScope[strlen($3)] = '\0'; currClass = gScope; free($3);}  corp_clasa

corp_clasa : '{' declaratii_variabile METHODS {currScope = _method;} declaratii_functii '}' ';'
           | '{' declaratii_variabile '}' ';'
           ;

main : type MAIN_FUN {strcpy(gScope, "main");}'(' ')' '{' statements '}'
     ;

statements : 
           | statements statement
           ;

statement : IF expression    { if($2->type != _bool) yyerror("Expression not a bool!");} '{' {strcat(gScope, ">>if");} statements '}' {} { gScope[strlen(gScope) - 4] = '\0'; } else
          | WHILE expression { if($2->type != _bool){yyerror("Expression not a bool!");}}'{' { strcat(gScope, ">>while");} statements '}' { gScope[strlen(gScope) - 7] = '\0'; }
          | FOR '(' expression_list ';' expression { if($5->type != _bool){yyerror("Expression not a bool!");}}';' expression_list ')' '{' { strcat(gScope, ">>for"); } statements '}' { gScope[strlen(gScope) - 5] = '\0'; }
          | expression_list ';'
          | declaratie_v
          | EVAL ':' expression  ':'    { printf("The expression at the line %i evaluates to: ", yylineno);
                                            switch($3->type){
                                                case _bool: printf("%s", *((int*)$3->value) == 1 ? "true" : "false"); break;
                                                case _int: printf("%i", *((int*)$3->value)); break;
                                                case _float: printf("%f", *((float*)($3->value))); break;
                                                case _char: printf("%c", *((char*)($3->value))); break;
                                                case _string: printf("%s", (char*)($3->value));} printf("\n"); }
          | TYPEOF ':' expression ':'   { printf("The expression at the line %i has type: %s!\n", yylineno, (storeCharPtr = getTypeAsString(0, $3->type))); free(storeCharPtr);}
          | RETURN expression ';'       { if($2->type != funcRet) yyerror("Different return type!");}
          ;

else : /* epsilon */
     | ELSE '{' { strcat(gScope, ">>else");} statements '}' { gScope[strlen(gScope) - 6] = '\0';}
     ;

expression_list : expression
                | expression_list ',' expression
                | assignement
                | expression_list ',' assignement
                ;

assignement :  ids '=' expression       { if($1->isConst){ sprintf(errorBuffer, "Variable %s is a constant!", $1->name); yyerror(errorBuffer); }
                                            if($1->type != $3->type){ sprintf(errorBuffer, "Variable %s has type %i, while the value of expression has type %i!\n", $1->name, $1->type, $3->type); yyerror(errorBuffer); } $1->value = copyValue($3->value, $1->type); }
            ;

expression : constants                      { $$ = $1;}
           | ids                            { if($1->value != 0){ $$=buildAST($1->value, 0, 0, $1->type); } 
                                                else{ sprintf(errorBuffer, "Variable %s is unitialized!", $1->name);yyerror(errorBuffer);}}
           | expression '+' expression      { if($1->type == $3->type) {
                                                if($1->type == _int){
                                                    int tmp = *((int*)($1->value)) + *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _int);  if(ENABLE_PRINT_OPERATIONS) printf("%i + %i\n", *((int*)($1->value)), *((int*)($3->value)));}
                                                        else if($1->type == _float) {float tmp = *((float*)($1->value)) + *((float*)($3->value)); $$=buildAST(&tmp, $1, $3, _float); if(ENABLE_PRINT_OPERATIONS) printf("%f + %f\n", *((float*)($1->value)), *((float*)($3->value)));}
                                                            else yyerror("The operands are not arithmetical!");} else yyerror("The operands don't have the same type!");}
           | expression '-' expression      { if($1->type == $3->type) {
                                                if($1->type == _int){
                                                    int tmp = *((int*)($1->value)) - *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _int); if(ENABLE_PRINT_OPERATIONS) printf("%i - %i\n", *((int*)($1->value)), *((int*)($3->value)));}
                                                        else if($1->type == _float) {float tmp = *((float*)($1->value)) - *((float*)($3->value)); $$=buildAST(&tmp, $1, $3, _float); if(ENABLE_PRINT_OPERATIONS) printf("%f - %f\n", *((float*)($1->value)), *((float*)($3->value)));}
                                                            else yyerror("The operands are not arithmetical!");} else yyerror("The operands don't have the same type!");}
           | expression '*' expression      { if($1->type == $3->type) {
                                                if($1->type == _int){
                                                    int tmp = *((int*)($1->value)) * *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _int); if(ENABLE_PRINT_OPERATIONS) printf("%i * %i\n", *((int*)($1->value)), *((int*)($3->value)));}
                                                        else if($1->type == _float) {float tmp = *((float*)($1->value)) * *((float*)($3->value)); $$=buildAST(&tmp, $1, $3, _float); if(ENABLE_PRINT_OPERATIONS) printf("%f * %fi\n", *((float*)($1->value)), *((float*)($3->value)));}
                                                            else yyerror("The operands are not arithmetical!");} else yyerror("The operands don't have the same type!");}
           | expression '/' expression      { if($1->type == $3->type) {
                                                if($1->type == _int){
                                                    if(*((int*)($3->value)) == 0) yyerror("Division by zero!");
                                                    int tmp = *((int*)($1->value)) / *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _int); if(ENABLE_PRINT_OPERATIONS) printf("%i / %i\n", *((int*)($1->value)), *((int*)($3->value)));}
                                                        else if($1->type == _float) {float tmp = *((float*)($1->value)) / *((float*)($3->value)); $$=buildAST(&tmp, $1, $3, _float); if(ENABLE_PRINT_OPERATIONS) printf("%f / %f\n", *((float*)($1->value)), *((float*)($3->value)));}
                                                            else yyerror("The operands are not arithmetical!");} else yyerror("The operands don't have the same type!");}
           | expression '%' expression      { if($1->type == $3->type) {
                                                if($1->type == _int && $3->type == _int){
                                                    if(*((int*)($3->value)) == 0) yyerror("Modulo by zero!");
                                                    int tmp = *((int*)($1->value)) % *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _int); if(ENABLE_PRINT_OPERATIONS) printf("%i %% %i\n", *((int*)($1->value)), *((int*)($3->value)));}
                                                            else yyerror("Not an integer!");} else yyerror("The operands don't have the same type!");}
           | '-' expression %prec UNNOT     { if($2->type == _int){ 
                                                *((int*)($2->value)) -= 2 * *((int*)($2->value)); printf("%i\n", *((int*)$2->value)); $$=$2;
                                                    } else if($2->type == _float) {*((float*)($2->value)) -= 2 * *((float*)($2->value)); printf("%f\n", *((float*)$2->value)); $$=$2;}
                                                        else yyerror("The operand is not arithmetical!");}
           | expression EQ expression       { if($1->type == $3->type) {
                                                if($1->type == _int){
                                                    int tmp = *((int*)($1->value)) == *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                        else if($1->type == _float) {int tmp = *((float*)($1->value)) == *((float*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                            else yyerror("The operands are not arithmetical!");} else yyerror("The operands don't have the same type!");}
           | expression NEQ expression      { if($1->type == $3->type) {
                                                if($1->type == _int){
                                                    int tmp = *((int*)($1->value)) != *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                        else if($1->type == _float) {int tmp = *((float*)($1->value)) != *((float*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                            else yyerror("The operands are not arithmetical!");} else yyerror("The operands don't have the same type!");}
           | expression LEQ expression      { if($1->type == $3->type) {
                                                if($1->type == _int){
                                                    int tmp = *((int*)($1->value)) <= *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                        else if($1->type == _float) {int tmp = *((float*)($1->value)) <= *((float*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                            else yyerror("The operands are not arithmetical!");} else yyerror("The operands don't have the same type!");}
           | expression GEQ expression      { if($1->type == $3->type) {
                                                if($1->type == _int){
                                                    int tmp = *((int*)($1->value)) >= *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                        else if($1->type == _float) {int tmp = *((float*)($1->value)) >= *((float*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                            else yyerror("The operands are not arithmetical!");} else yyerror("The operands don't have the same type!");}
           | expression '<' expression      { if($1->type == $3->type) {
                                                if($1->type == _int){
                                                    int tmp = *((int*)($1->value)) < *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                        else if($1->type == _float) {int tmp = *((float*)($1->value)) < *((float*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                            else yyerror("The operands are not arithmetical!");} else yyerror("The operands don't have the same type!");}
           | expression '>' expression      { if($1->type == $3->type) {
                                                if($1->type == _int){
                                                    int tmp = *((int*)($1->value)) > *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                        else if($1->type == _float) {int tmp = *((float*)($1->value)) > *((float*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                            else yyerror("The operands are not arithmetical!");} else yyerror("The operands don't have the same type!");}
           | expression OR expression       { if($1->type == $3->type) {
                                                if($1->type == _bool){
                                                    int tmp = *((int*)($1->value)) || *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                        else yyerror("The operands are not boolean!");} else yyerror("The operands don't have the same type!");}
           | expression AND expression      { if($1->type == $3->type) {
                                                if($1->type == _bool){
                                                    int tmp = *((int*)($1->value)) && *((int*)($3->value)); $$=buildAST(&tmp, $1, $3, _bool);}
                                                        else yyerror("The operands are not boolean!");} else yyerror("The operands don't have the same type!");}
           | '!' expression %prec NOT       { if($2->type == _bool){ *((int*)($2->value)) = ! *((int*)($2->value)); $$=$2; }   
                                                    else yyerror("The operand is not a bool!");}
           | expression APPEND expression   { if($1->type == $3->type) {
                                                if($1->type == _string){
                                                    char* tmp = malloc(strlen((char*)($1->value)) + strlen((char*)($3->value)));
                                                    strcpy(tmp, (char*)($1->value)); strcat(tmp, (char*)($3->value)); $$=buildAST(tmp, $1, $3, _string); free(tmp);}
                                                        else yyerror("The operands are not boolean!");} else yyerror("The operands don't have the same type!");}
           | '(' expression ')'             { $$=$2; }
           ;

constants : BOOLEAN         { $$=buildAST(&$1, 0, 0, _bool);}
          | INTEGER         { $$=buildAST(&$1, 0, 0, _int);}
          | CFLOAT          { $$=buildAST(&$1, 0, 0, _float);}
          | CSTRING         { $$=buildAST($1, 0, 0, _string); free($1);}
          | CHARACTER       { $$=buildAST(&$1, 0, 0, _char);}
          ;

ids : ID                            {varDefined($1); $$=getVar($1); free($1);}
    | ID '[' INTEGER ']'            {varDefined($1); $$=getVar($1); free($1);}
    | function_call                 
    | ID '.' ids
    | ID '[' INTEGER ']' '.' ids
    ;

function_call : ID '(' lista_apel ')'                { struct symVar* toValidate = getFun($1)->parameters;
                                                        while(toValidate != 0 && $3 != 0){ if(toValidate->type != $3->type) yyerror("Function call doesn't matches the time of function parameters!");
                                                            toValidate = toValidate->nextVar; $3 = $3->next; } if((void*)toValidate != (void*)$3) yyerror("Function call with wrong number of parameters!");}                 
              ;


lista_apel : /* epsilon */                          { $$ = 0;}
           | lista_apel_expr                        { $$ = $1;}
           ;

lista_apel_expr : expression                        { struct paramType* el = malloc(sizeof(struct paramType)); el->type = $1->type; el->next = 0; $$=el; }
                | lista_apel_expr ',' expression    { struct paramType* el = malloc(sizeof(struct paramType)); el->type = $3->type; el->next = $1; $$=el; }
                ;



%%

int main(int argc, char* argv[]){
    FILE *fFile, *vFile;

    if(!(yyin = fopen(argv[1], "r"))){
        fprintf(stderr, "Can't access %s!\n", argv[1]);
        exit(1);
    }

    gScope = allocMeC(128, char); 
    currClass = allocMeC(32, char);
    strcpy(gScope, "global");
    errorBuffer = allocMeC(128, char);

    yyparse();
    
// Printing Variables Table
    struct symVar* varFree;
    if(!(vFile = fopen(VARIABLES_FILE, "w"))){
        fprintf(stderr, "Can't access %s!\n", VARIABLES_FILE);
        exit(1);
    }
    while(stVar){
        varFree = stVar;
        fprintf(vFile, "%s ", 
                (storeCharPtr = getTypeAsString(stVar->isConst, stVar->type)));
        fprintf(vFile, "%s", stVar->name);
        if(stVar->isVec)
            fprintf(vFile, "[%i]", stVar->isVec);
        if(stVar->value)
            switch(stVar->type){
                case _bool: fprintf(vFile, " = %s", *((int*)stVar->value) == 1 ? "true" : "false"); break;
                case _int: fprintf(vFile, " = %i", *((int*)stVar->value)); break;
                case _float: fprintf(vFile, " = %f", *((float*)(stVar->value))); break;
                case _char: fprintf(vFile, " = '%c'", *((char*)(stVar->value))); break;
                case _string: fprintf(vFile, " = \"%s\"", (char*)(stVar->value));}
        else fprintf(vFile, " is unitialized");
        fprintf(vFile, " \n\t\t\tscope: %s", stVar->scope);
        fprintf(vFile, ";\n\n");
        stVar = stVar->nextVar;
        free(storeCharPtr); free(varFree);
    }fclose(vFile);

// Printing Functions Table
    struct symFun* funFree;
    if(!(fFile = fopen(FUNCTIONS_FILE, "w"))){
        fprintf(stderr, "Can't access %s!\n", FUNCTIONS_FILE);
        exit(1);
    }
    while(stFun){
        funFree = stFun;
        fprintf(vFile, "%s ", 
                (storeCharPtr = getTypeAsString(stFun->isConst, stFun->type))); free(storeCharPtr);
        fprintf(fFile, "%s(", stFun->name);
        while(stFun->parameters){
            struct symVar* varFree = stFun->parameters;
            fprintf(vFile, "%s ", 
                    (storeCharPtr = getTypeAsString(stFun->parameters->isConst, stFun->parameters->type)));
            fprintf(fFile, "%s",stFun->parameters->name);
            stFun->parameters = stFun->parameters->nextVar;
            if(stFun->parameters != NULL) fprintf(fFile, ", ");
            free(varFree); free(storeCharPtr);
        }
        fprintf(fFile, ");\n");
        stFun = stFun->nextFun;
        free(funFree);
    }fclose(fFile);
    free(gScope);
}

