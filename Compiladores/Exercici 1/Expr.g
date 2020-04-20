grammar Expr; //

root : expr EOF ;

expr :
      <assoc=right> expr '^' expr
    | expr MUL expr
    | expr DIV expr
    | expr SUM expr
    | expr SUB expr
    | NUM
    ;

MUL : '*';
DIV : '/';
SUM : '+';
SUB : '-';
NUM : [0-9]+;
WS  : [ \n]+ -> skip;

// WORD : [a-zA-Z\u0080-\u00FF]+ ; // Para el uso de acentos