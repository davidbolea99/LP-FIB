grammar Expr; //

root : expr EOF ;

expr :
      <assoc=right> expr powop expr
    | expr op expr
    | NUM
    ;

powop: POW;

op : 
    MUL
  | DIV
  | SUM
  | SUB
;

POW : '^';
MUL : '*';
DIV : '/';
SUM : '+';
SUB : '-';
NUM : [0-9]+;
WS  : [ \n]+ -> skip;

// WORD : [a-zA-Z\u0080-\u00FF]+ ; // Para el uso de acentos