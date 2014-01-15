import pickle

DELIMETERS = ['(',')']
QUOTE = ['"']
COMMENT = [';']
WHITESPACE = ['\t', ' ', '\n']
ALL_SPECIAL = DELIMETERS + QUOTE + COMMENT + WHITESPACE
INDENT = 8

def skip_comment(text, point):
    """ 
    text[point] is a COMMENT character, return the position of the next newline.
    """
    while point < len(text) and text[point] != '\n':
        point += 1
    return point , 0

def add_tab(cur_indent):
    """ 
    return the new value of indent when a tab is hit
    """
    cur_indent += 1
    while cur_indent % INDENT != 0:
        cur_indent += 1
    return cur_indent

def tokenizer(text):
    """ 
    return a list of tokens from a string.
    """
    i = 0
    prev_indent, curr_indent = 0, 0
    while i < len(text):
        if text[i] in DELIMETERS:
            token = text[i]
            yield token, prev_indent
            curr_indent += 1
        elif text[i] in QUOTE:
            token = text[i]
            i += 1
            curr_indent += 1
            while i < len(text) and text[i] not in QUOTE:
                token += text[i]
                if text[i] == '\\':
                    token += text[i+1]
                    i += 1
                    curr_indent += 1
                if text[i] == '\t':
                    curr_indent = add_tab(curr_indent)
                i += 1
                curr_indent += 1
            token += text[i]
            curr_indent += 1
            yield token, prev_indent
        elif text[i] in COMMENT:
            i, curr_indent = skip_comment(text, i)
            continue
        elif text[i] in WHITESPACE:
            token = ''
            while i < len(text):
                if text[i] in WHITESPACE:
                    if text[i] == '\n':
                        curr_indent = 0
                    elif text[i] == '\t':
                        curr_indent = add_tab(curr_indent)
                    else:
                         curr_indent += 1
                    token += text[i]
                    i += 1
                   
                elif text[i] in COMMENT:
                    i, curr_indent = skip_comment(text,i)
                else:
                    break
            yield token, prev_indent
            prev_indent = curr_indent
            continue
        else: # look for the next special character
            token = ''
            while i < len(text) and text[i] not in ALL_SPECIAL:
                token += text[i]
                i += 1
                curr_indent += 1
            yield token, prev_indent
            prev_indent = curr_indent
            continue
        i += 1 
        prev_indent = curr_indent

class Tree:
    """
    tree has either children or string
    """
    def __init__(self, entry=('',0)):
        self.string = entry[0]
        self.indent = entry[1]
        self.children = []
        self.space = ''
        self.right = -1

    def __repr__(self):
        return self.PrintTree(0)
    def PrintNode(self):
        return 'Tree(str=%s indent=%d rightindent = %d space="%s")' % \
            (repr(self.string), self.indent, self.right, repr(self.space))

    def PrintTree(self, level):
        indent = ''
        for i in range(level):
            indent = indent + '  '
        output = indent + self.PrintNode() + '\n'
        for child in self.children:
            output = output + Tree.PrintTree(child, level + 1)
        return output
    
def read(tokens):
    """ read and return single element
    """
    try:
        space = ''
        val = next(tokens)
        if val[0][0] in WHITESPACE:
            space, val = val[0], next(tokens)
        if val[0] not in ALL_SPECIAL:
            el = Tree(val)
            el.space = space
            return el
        elif val[0] == '(':
            el = read_list(tokens)
            el.space = space
            el.indent = val[1]
            return el
        else:
            #print val
            return val[1]
    except StopIteration:
        return None
def read_list(tokens):
    """return list of items
    """
    t = Tree()
    val = read(tokens)
    while val and type(val)!= int:
        t.children.append(val)
        val = read(tokens)
    if type(val) == int:
        t.right = val
    return t

def check_syntax(tree, cur_indent = 0):
    if not tree.children: # if node
        if tree.indent <= cur_indent: # children can't be less indented than parent
            #print Tree.PrintNode(child)
            return False
        return True
    else:
        cur_indent = tree.indent
        i = 0
        min_indent = tree.children[0].indent
        min_right = tree.right
        for child in tree.children:
            correct = check_syntax(child, cur_indent)
            if min_indent > child.indent:
                #print Tree.PrintNode(child)
                return False
            if min_right < child.indent and min_right > -1:
                #print min_right, 'min_right'
                #print child.indent, 'child.indent'
                #print Tree.PrintNode(child)
                return False
            if not correct:
                #print Tree.PrintNode(child)
                return False
        return True

test = "(A B)"
test1 = "(A (B C\n D)\n)"
test2 = "(A (B C\n    D)\n)"
test3 = "(A (B C\n D)\n )"
test4 = "(A (B\n     ))"
test5 = "(A (B C D)\n )"
test6 = "(A (B\n))"
test7 = "(A\n    )"
tests = [test, test1, test2, test3, test4, test5, test6, test7]
#tests= [test6, test7]

for test in tests:
    print
    print test
    tree = read_list(tokenizer(test))
    print check_syntax(tree)

"""            
f = open('wimpy-delete.el','r')
count = 1
whole = ''
for line in f:
    if count > -1 and count < 42:
        whole += line
    count += 1
#print repr(whole)
#q = tokenizer(whole)
#c = read_list(q)
#print c
#print check_syntax(c)

"""
