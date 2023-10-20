#define FUNCTIONS_FILE "symbol_table_functions.txt"
#define VARIABLES_FILE "symbol_table.txt"

enum Type{
    _int,
    _float,
    _bool,
    _char,
    _string
};

enum Scope{
    _global,
    _class,
    _function,
    _method,
    _main
};

struct symVar{
    int line;
    int type;
    int isConst;
    int isVec;
    char* name;
    void* value;
    char* scope;
    struct symVar* nextVar;
};

void symlook(struct symVar*);
struct symVar* getVar(const char*);

struct symFun{
    int line;
    int type;
    int isConst;
    char* name;
    struct symVar* parameters;
    struct symFun* nextFun;
};

void funlook(struct symFun*);
struct symFun* getFun(const char*);

struct AST{
    void* value;
    struct AST* left;
    struct AST* right;
    int type;
};

struct AST* buildAST(void*, struct AST*, struct AST*, int);

struct paramType{
    int type;
    struct paramType* next;
};

char* getTypeAsString(int, int);
void* copyValue(void*, int);
void Eval(char*);
int Typeof(char*);