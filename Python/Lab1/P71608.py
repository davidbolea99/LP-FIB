class Tree:
    def __init__(self, x):
        self.rt = x
        self.child = []
 
    def add_child(self, a):
        self.child.append(a)
 
    def root(self):
        return self.rt
 
    def ith_child(self, i):
        return self.child[i]
 
    def num_children(self):
        return len(self.child)

class Pre(Tree):

    def preorder(self):

        if self.num_children == 0:
            return self.rt
        
        R = [self.rt]

        for c in self.child:
            if not isinstance(c,Pre):
                R.append(c)
            else:
                R += c.preorder()

        return R


# TESTING
# t = Pre(2)
# t.add_child(Pre(3))
# t.add_child(Pre(4))
# print(t.num_children())
# t.ith_child(1).add_child(Pre(5))
# print(t.preorder())