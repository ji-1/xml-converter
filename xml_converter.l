%{


extern int yylval;
#define NLEXEM 9999

#include <stdlib.h>
#include <string.h>
typedef struct Token {
  int tok_name;  
  union {
    int int_val;
    double float_val;
    char* ID_val;
    char op_val;
  }tok_val;   
} TOKEN;

typedef struct node {
  char name[15];
  TOKEN token;
  struct node *child1;
  struct node *child2;
} NODE;

NODE* expression();
NODE* factor();
NODE* assignment();
NODE* makenode(char *op, NODE *tmp1, NODE *tmp2);
NODE *root;

TOKEN tok_list[NLEXEM];

typedef enum{
  INT=1,
  FLOAT,
  PLUS,
  MINUS,
  MULT,
  DIV,
  ASSIGN,
  ID,
  LPAREN,
  RPAREN,
  LEXERR,
  UMINUS,
  SYNTAXERR,
  EOL,
  NOTOKEN
} Symbol;
Symbol sym;

%}
%option yylineno
ws                    [ \t]+

letter                [A-Za-z]
digit                 [0-9]
eol                   \n
ID                ({letter}({letter}|{digit}|_)*)|(_+({letter}|{digit})({letter}|{digit}|_)*)[^\.]
integer               [0-9]+
float                 (\.{digit}+([eE][-+]?)?{digit}+)|({digit}+\.{digit}*([eE][-+]?{digit}+)?)|({digit}+([eE][-+]?){digit}+)

%%

{ws} 
"\n"                  { return EOL; }
"+"                   { return PLUS; }
"-"                   { return MINUS;    }
"*"                   { return MULT;}
"/"                   { return DIV;}
"="                   { return ASSIGN;}

"("                   { return LPAREN;}
")"                   {return RPAREN;}

{ID}            { return ID;          }          
{float}              { return FLOAT;       }
{integer}            { return INT;         }
{integer}{letter}    { return LEXERR;      }

. {   return LEXERR; }

%%

int yylval, idx=-1,line_cnt,line_idx=0,tok_cnt=0,syntaxerr=0;



void 
error (const char msg[]) {
  printf("%s\n",msg);
}

int 
nextSym() {
  if (idx+1!= tok_cnt) {
    return tok_list[++idx].tok_name;
  } else {
    return NOTOKEN;
  }
}

NODE *
resterm(NODE *node) {
  sym = nextSym();
  NODE* tmp1;
  NODE* tmp2;
  switch(sym) {
    case MULT:
      tmp1= factor();
      tmp2 = resterm(tmp1);
      return makenode("mult", node, tmp2);
    case DIV:
      tmp1= factor();
      tmp2 = resterm(tmp1);
      return makenode("div", node, tmp2);
    default:
      idx--;
      tok_cnt++;
      return node;  
  }
}
NODE *
makenode(char *op, NODE *tmp1, NODE *tmp2){
  NODE *node = malloc(sizeof(NODE));
  strcpy(node->name,op);
  if (tmp1)
    node->child1=tmp1;
  if (tmp2)
    node->child2=tmp2;
  return node;
}

NODE *
factor() {
  NODE *tmp = malloc(sizeof(NODE));
  sym = nextSym();
  switch(sym) {
    case LPAREN:
      free(tmp);
      NODE *node = expression();
      sym=nextSym();
      if (sym==RPAREN)
        return node;
      else {
        idx-=2;
        return NULL;
      }
    case ID:
      strcpy(tmp->name,"id");
      tmp->token=tok_list[idx];
      return tmp;
    case INT:
      strcpy(tmp->name,"int");
      tmp->token=tok_list[idx];
      return tmp;
    case FLOAT:
      strcpy(tmp->name,"float");
      tmp->token=tok_list[idx];
      return tmp;
    case MINUS:
      //strcpy(tmp->name,"uminus");
      free(tmp);
      NODE *tmp2=expression();
      return makenode("uminus", tmp2, NULL);
    default:
      syntaxerr = 1;
      free(tmp);
      return NULL;

  }
}


NODE *
term() {
  NODE *tmp = factor();
  return resterm(tmp);
}

NODE *
restexp(NODE *node) {
  sym = nextSym();
  NODE* tmp1;
  NODE* tmp2;
  switch(sym) {
    case PLUS:
      tmp1 = term();
      tmp2 = restexp(tmp1);
      return makenode("plus", node, tmp2);
    case MINUS:
      tmp1 = term();
      tmp2 = restexp(tmp1);
      return makenode("minus", node, tmp2);
    default:
      idx--;
      return node;
  }
}

NODE *
expression() {
  NODE *tmp = term();
  return restexp(tmp);
}


NODE * 
assignment() {
  sym = nextSym();
  NODE *tmp1;
  NODE *tmp2;
  switch(sym) {
    case ID:
      tmp1 = malloc(sizeof(NODE));
      strcpy(tmp1->name,"id");
      tmp1->token=tok_list[idx];
      sym=nextSym();
      if (sym==ASSIGN) {
        tmp2 =  expression();
        return makenode("assign",tmp1,tmp2);
      } else {
        syntaxerr=1;
        free(tmp1);
        free(tmp2);
        return NULL;
      }
    default:
      syntaxerr=1;
      return NULL;
  }
}

void print_node(NODE *node, int deep) {
  int i=0;
  if (!strcmp(node->name,"id")) {
    for(i=0;i<deep;i++) printf("  ");
    printf("<%s>%s</%s>\n", node->name,node->token.tok_val.ID_val,node->name);
  } else if (!strcmp(node->name,"int")) {
    for(i=0;i<deep;i++) printf("  ");
    printf("<%s>%d</%s>\n", node->name,node->token.tok_val.int_val,node->name);
  } else if (!strcmp(node->name,"float")) {
    for(i=0;i<deep;i++) printf("  ");
    printf("<%s>%.2f</%s>\n", node->name,node->token.tok_val.float_val,node->name);
  } else if (!strcmp(node->name, "plus")||!strcmp(node->name, "minus")||!strcmp(node->name, "uminus")
    ||!strcmp(node->name, "plus")|| !strcmp(node->name, "mult")||!strcmp(node->name, "div")||!strcmp(node->name, "assign")) {
    for(i=0;i<deep;i++) printf("  ");
      printf("<%s>\n", node->name);
    if (node->child1)
      print_node(node->child1, deep+1);
    if (node->child2)
      print_node(node->child2, deep+1);
    for(i=0;i<deep;i++) printf("  ");
      printf("</%s>\n", node->name);
  } 
    return;
}

int 
main(void)
{
  int tok,i,j=0,i_pos=0, lexerr=0,assign=0;
  for(i=0;(tok=yylex())!=0;) {
    tok_list[i].tok_name = tok;
    switch (tok) {
      case ID:
        tok_list[i++].tok_val.ID_val = strdup(yytext);
        break;
      case INT:
        tok_list[i++].tok_val.int_val = atoi(yytext);
        break;
      case FLOAT:
        tok_list[i++].tok_val.float_val = strtod(yytext, NULL);
        //tok_list[i++].tok_val.float_val = atof(yytext);
        break;
      case DIV:
      case MULT:
      case PLUS:
      case MINUS:
      case LPAREN:
      case RPAREN:
        tok_list[i++].tok_val.op_val = yytext[0];
        break;
      case ASSIGN:
        tok_list[i++].tok_val.op_val = yytext[0];
        assign=1;
        break;
      case LEXERR:
        lexerr=1;
        break;

      case EOL:
        tok_cnt=i;
        if (tok_cnt==0) continue;
        if (!lexerr) {
          if (assign) {
            root = assignment();
            assign = 0;
          } else {
            root = expression();
          }
          if (syntaxerr) {
            error("error: syntax error");
            syntaxerr=0; 
          } else {
            print_node(root, 0);
          }
        } else {
          error("error: lexical error");
          lexerr=0;
        }

        memset(tok_list,0,tok_cnt);
        i=0;
        idx=-1;
        root = NULL;
        printf("--------------------\n");
        continue;
    }
  }

}
