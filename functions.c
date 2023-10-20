#include <string.h>
#include <stdio.h>
#include <stdlib.h>


#include "symbols.h"

struct symVar* stVar = (struct symVar*) 0;
struct symVar* lastVar = (struct symVar*) 0;
struct symFun* stFun = (struct symFun*) 0;
struct symFun* lastFun = (struct symFun*) 0;

extern FILE* yyin;
extern int yylineno;
extern int cInteger;

int Const, Type, funcRet;

char* errorBuffer;
char* gScope;
int currScope;
char* currClass;
char *storeCharPtr;


void yyerror(char* msg){
    fprintf(stderr, "Line %d: %s\n", yylineno, msg);
    exit(1);
}

char* constructMethodName(const char* cName, const char* mName){
    char* methodName = malloc(strlen(cName) + strlen(mName) + 2);
    strcpy(methodName, cName);
    strcat(methodName, "::");
    strcat(methodName, mName);
    return methodName;
}

void* copyValue(void* toCopy, int hasType){
    void* copyLoc;
    int lenght;
    switch(hasType){
        case _bool: case _int:
            copyLoc = malloc(sizeof(int));
            memcpy(copyLoc, toCopy, sizeof(int));
            break;
        case _float:
            copyLoc = malloc(sizeof(float));
            memcpy(copyLoc, toCopy, sizeof(float));
            break;
        case _char:
            copyLoc = malloc(sizeof(char));
            memcpy(copyLoc, toCopy, sizeof(char));
            break;
        case _string:
            lenght = strlen((char*)toCopy);
            copyLoc = malloc(lenght);
            memcpy(copyLoc, toCopy, lenght);
    }
    return copyLoc;
}

struct AST* buildAST(void* val, struct AST* l, struct AST* r, int t){
    struct AST* node = malloc(sizeof(struct AST));
    node->type = t;
    node->left = l;
    node->right = r;
    node->value = copyValue(val, t);
    return node;
}

void symlook(struct symVar* newVar){
    if(stVar == 0){
        lastVar = newVar;
        stVar = newVar;
    }else{
        struct symVar* sym = stVar;
        do{
            if(!strcmp(sym->name, newVar->name)){
                if(!strcmp(sym->scope, newVar->scope) || 
                    !strcmp(newVar->scope, "global") || !strcmp(sym->scope, "global")){

                    sprintf(errorBuffer, "Variable already defined at line: %i!", sym->line);
                    yyerror(errorBuffer);
                }
            }
            sym = sym->nextVar;
        }while(sym);
            lastVar->nextVar = newVar;
            lastVar = newVar;
    }
}

void myStrrev(char* toReverse){
    size_t len = strlen(toReverse) - 1, i = 0;
    while(i <= len / 2){
        char aux = toReverse[i];
        toReverse[i] = toReverse[len - i];
        toReverse[len - i] = aux;
        i++;
    }
}

struct symVar* getVar(const char* myVar){
    struct symVar* sym = stVar;
    size_t len = 0;
    while(sym){
        if(!strcmp(sym->name, myVar)){
            if(!strcmp(sym->scope, gScope) || !strcmp(sym->scope, "global"))
                return sym;

            char* goBack = strdup(gScope), *p;
            myStrrev(goBack);
            p = strtok(goBack, ">");
            while(p){
                myStrrev(p);
                len += strlen(p);
                if(!strncmp(sym->scope, gScope, strlen(gScope) - strlen(p) - 2))
                    return sym;
                p = strtok(0, ">");
            }
        }
        sym = sym->nextVar;
    }
    sprintf(errorBuffer, "Variable %s was not declared in %s!", myVar, gScope);
    yyerror(errorBuffer);
}

void varDefined(const char* toCheck){
    struct symVar* sym = stVar;
    while(sym){
        if(!strcmp(sym->name, toCheck))
            return;
        sym = sym->nextVar;
    }
    sprintf(errorBuffer, "Variable %s was not declared!", toCheck);
    yyerror(errorBuffer);
}

struct symFun* getFun(const char* myFun){
    struct symFun* sym = stFun;
    while(sym){
        if(!strcmp(sym->name, myFun)){
                return sym;
        }
        sym = sym->nextFun;
    }
    sprintf(errorBuffer, "Function %s() was not declared!", myFun);
    yyerror(errorBuffer);
}

void funDefined(const char* toCheck){
    struct symFun* fun = stFun;
    while(fun){
        if(!strcmp(fun->name, toCheck))
            return;
        fun = fun->nextFun;
    }
    sprintf(errorBuffer, "Function %s() was not declared!", toCheck);
    yyerror(errorBuffer);
}

void funlook(struct symFun* myFun){
#if ENABLE_PRINTS == 2
    printf("Received %s %i %i with parameters: \n", myFun->name, myFun->isConst, myFun->type);
    printVarss(myFun->parameters);
#endif
    if(stFun == 0){ 
        stFun = myFun; 
        lastFun = myFun; 
    }else{
        struct symFun* sym = stFun;
        struct symVar *lFun, *nFun;
        do{
            if(!strcmp(sym->name, myFun->name)){
                lFun = sym->parameters, nFun = myFun->parameters;
                while(lFun != 0 && nFun != 0){
                    if(lFun->type != nFun->type || 
                       lFun->isConst != nFun->isConst) break;
                    lFun = lFun->nextVar;
                    nFun = nFun->nextVar;
                }
                if(lFun == 0 && nFun == 0){
                    sprintf(errorBuffer, "Function already defined at line: %i!", sym->line);
                    yyerror(errorBuffer);
                }
            }
            sym = sym->nextFun;
        }while(sym);
        lastFun->nextFun = myFun;
        lastFun = myFun;
    }
#if ENABLE_PRINTS == 2
    printf("Added %s %i %i \n", myFun->name, myFun->isConst, myFun->type);
#endif
}

void printVars(){
    struct symVar* aux = stVar;
    while(aux){
        printf("%s %i %i\n", aux->name, aux->isConst, aux->type);
        aux = aux->nextVar;
    }
}

void printVarss(struct symVar* l){
    struct symVar* aux = l;
    while(aux){
        printf("%s %i %i\n", aux->name, aux->isConst, aux->type);
        aux = aux->nextVar;
    }
}

void printFunc(){
    struct symFun* aux = stFun;
    while(aux){
        printf("%s %i %i with param:\n", aux->name, aux->isConst, aux->type);
        printVarss(aux->parameters);
        aux = aux->nextFun;
    }
}

void PrintMyFunc(struct symFun* l){
    struct symFun* aux = l;
     while(aux){
        printf("%s %i %i with param:\n", aux->name, aux->isConst, aux->type);
        printVarss(aux->parameters);
        aux = aux->nextFun;
    }
}

char* getTypeAsString(int constFlag, int typeFlag){
    char *strType = malloc(13 * sizeof(char));       // lenght of "const" ++ " " ++ "string"
    if(constFlag) strcat(strType, "const ");
    
    switch(typeFlag){
            case _bool:   strcat(strType, "bool");
                break;
            case _int:    strcat(strType, "int");
                break;
            case _float:  strcat(strType, "float");
                break;
            case _char:   strcat(strType, "char");
                break;
            case _string: strcat(strType, "string");
    }return strType;
}