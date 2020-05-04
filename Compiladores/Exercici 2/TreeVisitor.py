if __name__ is not None and "." in __name__:
    from .ExprParser import ExprParser
    from .ExprVisitor import ExprVisitor
else:
    from ExprParser import ExprParser
    from ExprVisitor import ExprVisitor

class TreeVisitor(ExprVisitor):
    
    def __init__(self):
        self.nivell = 0

    def visitExpr(self, ctx:ExprParser.ExprContext):
        
        l = [n for n in ctx.getChildren()]

        # Si solo hay un nodo, este es un numero
        if len(l) == 1: 
            n = l[0]
            print("| " * self.nivell +
                  ExprParser.symbolicNames[n.getSymbol().type] +
                  '(' +n.getText() + ')')
            self.nivell -= 1
        
        elif len(l) == 3:
            left = l[0]
            op = l[1]
            right = l[2]
            print('| ' *  self.nivell + '(' + op.getText() + ')')
            self.nivell += 1
            self.visit(left)
            self.nivell += 1
            self.visit(right)
            self.nivell -= 1