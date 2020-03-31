from collections import deque

class Tree:
    def __init__(self, x):
        self.rt = x
        self.child = []
        

    def __iter__(self):

        yield self.rt

        Q = deque(self.child)
        while not len(Q) == 0:
            ret = Q.popleft()
            Q += ret.child
            yield ret.rt

        raise StopIteration
 
    def addChild(self, a):
        self.child.append(a)
 
    def root(self):
        return self.rt
 
    def ithChild(self, i):
        return self.child[i]
 
    def num_children(self):
        return len(self.child)


# TESTING
# t = Tree(2)
# t.addChild(Tree(3))
# t.addChild(Tree(4))
# t.ithChild(0).addChild(Tree(5))
# print([x for x in t])