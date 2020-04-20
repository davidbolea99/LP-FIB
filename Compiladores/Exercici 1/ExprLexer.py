# Generated from Expr.g by ANTLR 4.7.1
from antlr4 import *
from io import StringIO
from typing.io import TextIO
import sys


def serializedATN():
    with StringIO() as buf:
        buf.write("\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\t")
        buf.write("\'\b\1\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7")
        buf.write("\4\b\t\b\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6\3\7\6")
        buf.write("\7\35\n\7\r\7\16\7\36\3\b\6\b\"\n\b\r\b\16\b#\3\b\3\b")
        buf.write("\2\2\t\3\3\5\4\7\5\t\6\13\7\r\b\17\t\3\2\4\3\2\62;\4\2")
        buf.write("\f\f\"\"\2(\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3")
        buf.write("\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\3\21\3\2")
        buf.write("\2\2\5\23\3\2\2\2\7\25\3\2\2\2\t\27\3\2\2\2\13\31\3\2")
        buf.write("\2\2\r\34\3\2\2\2\17!\3\2\2\2\21\22\7`\2\2\22\4\3\2\2")
        buf.write("\2\23\24\7,\2\2\24\6\3\2\2\2\25\26\7\61\2\2\26\b\3\2\2")
        buf.write("\2\27\30\7-\2\2\30\n\3\2\2\2\31\32\7/\2\2\32\f\3\2\2\2")
        buf.write("\33\35\t\2\2\2\34\33\3\2\2\2\35\36\3\2\2\2\36\34\3\2\2")
        buf.write("\2\36\37\3\2\2\2\37\16\3\2\2\2 \"\t\3\2\2! \3\2\2\2\"")
        buf.write("#\3\2\2\2#!\3\2\2\2#$\3\2\2\2$%\3\2\2\2%&\b\b\2\2&\20")
        buf.write("\3\2\2\2\5\2\36#\3\b\2\2")
        return buf.getvalue()


class ExprLexer(Lexer):

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    T__0 = 1
    MUL = 2
    DIV = 3
    SUM = 4
    SUB = 5
    NUM = 6
    WS = 7

    channelNames = [ u"DEFAULT_TOKEN_CHANNEL", u"HIDDEN" ]

    modeNames = [ "DEFAULT_MODE" ]

    literalNames = [ "<INVALID>",
            "'^'", "'*'", "'/'", "'+'", "'-'" ]

    symbolicNames = [ "<INVALID>",
            "MUL", "DIV", "SUM", "SUB", "NUM", "WS" ]

    ruleNames = [ "T__0", "MUL", "DIV", "SUM", "SUB", "NUM", "WS" ]

    grammarFileName = "Expr.g"

    def __init__(self, input=None, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.7.1")
        self._interp = LexerATNSimulator(self, self.atn, self.decisionsToDFA, PredictionContextCache())
        self._actions = None
        self._predicates = None


