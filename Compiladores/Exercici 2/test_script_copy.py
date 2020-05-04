import sys
from antlr4 import *
from ExprLexer import ExprLexer
from ExprParser import ExprParser

input_stream = InputStream(input('? '))                     # solicitar una linea
# input_stream = StdinStream()                                # stdin
# input_stream = FileStream(sys.argv[1])                      # archivo por parametro
# input_stream = FileStream(sys.argv[1], encoding='utf-8')    # archivo con acentos

lexer = ExprLexer(input_stream)
token_stream = CommonTokenStream(lexer)
parser = ExprParser(token_stream)
tree = parser.root()
print(tree.toStringTree(recog=parser))