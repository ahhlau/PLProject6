import ply.yacc as yacc

# Get the token map from the lexer.  This is required.
from lex import tokens

DEBUG = False

# Namespace & built-in functions

name = {}
variable = {}
#let = {}

def cons(l):
    return [l[0]] + l[1]

name['cons'] = cons

def concat(l):
    return l[0] + l[1]

name['concat'] = concat

def listar(l):
    return l

name['list'] = listar

def car(l):
    return l[0][0]

name['car'] = car

def cdr(l):
    return l[0][1:]

name['cdr'] = cdr

def eq(l):
    #variable[l[0]] = l[1]
    #print variable
    return l[0] == l[1]

name['eq'] = eq
name['='] = eq
#name['let'] = eq

def _and(l):
    return not False in l

name['and'] = _and

def _or(l):
    return True in l

name['or'] = _or

def cond(l):
    if l[0]:
        return l[1]
    else:
        return l[2]

name['cond'] = cond

def iff(l):
    if l[0]:
        return l[1]
    else:
        return l[2]
name['if'] = iff


# #####
# def let(l):
#     variable[l[0]] = l[0][1]
#     print variable[l[0]]
# name['let'] = let
# ####
#
# def let(l):
#     variable.clear()


def add(l):
    for i in range(len(l)):
        if type(l[i]) != int:
            l[i] = variable[l[i]]
            # print variable
    # variable.clear()
    return sum(l)

name['+'] = add

def minus(l):
    '''Unary minus'''
    for i in range(len(l)):
        if type(l[i]) != int:
            l[i] = variable[l[i]]
    return l[0] - l[1]

name['-'] = minus

def mult(l):
    for i in range(len(l)):
        if type(l[i]) != int:
            l[i] = variable[l[i]]
    return l[0] * l[1]

name['*'] = mult

def div(l):
    for i in range(len(l)):
        if type(l[i]) != int:
            l[i] = variable[l[i]]
    return l[0] / l[1]

name['/'] = div

def _print(l):
    print lisp_str(l[0])
    # print name

name['print'] = _print

#  Evaluation functions

def lisp_eval(simb, items):
    if simb in name:
        return call(name[simb], eval_lists(items))
    elif simb not in name:
        # print items
        variable[simb] = items[0]
        # print variable
        return items[-1]
    else:
        return [simb] + items

def call(f, l):
    try:
        return f(eval_lists(l))
    except TypeError:
        return f
# make an f that recognizes let, and stores the eval_lists(l) in an array that gets cleared at the end of the outer most expression
# make a grammar rule that recognizes end_expression: (let_expression binary_expression) and clears the stored value after the calculation of the return statement
def eval_lists(l):
    r = []
    for i in l:
        if is_list(i):
            if i:
                r.append(lisp_eval(i[0], i[1:]))
            else:
                r.append(i)
        else:
            r.append(i)
    return r

# Utilities functions

def is_list(l):
    return type(l) == type([])

def lisp_str(l):
    if type(l) == type([]):
        if not l:
            return "()"
        r = "("
        for i in l[:-1]:
            r += lisp_str(i) + " "
        r += lisp_str(l[-1]) + ")"
        variable.clear()
        return r
    elif l is True:
        return "#t"
    elif l is False:
        return "#f"
    elif l is None:
        return 'nil'
    else:
        return str(l)

# BNF

def p_exp_atom(p):
    'exp : atom'
    p[0] = p[1]

def p_exp_qlist(p):
    'exp : quoted_list'
    p[0] = p[1]

def p_exp_call(p):
    'exp : call'
    p[0] = p[1]

def p_quoted_list(p):
    'quoted_list : QUOTE list'
    p[0] = p[2]

def p_list(p):
    'list : LPAREN items RPAREN'
    p[0] = p[2]

def p_items(p):
    'items : item items'
    p[0] = [p[1]] + p[2]

def p_items_empty(p):
    'items : empty'
    p[0] = []

def p_empty(p):
    'empty :'
    pass

def p_item_atom(p):
    'item : atom'
    p[0] = p[1]

def p_item_list(p):
    'item : list'
    p[0] = p[1]

def p_item_list(p):
    'item : quoted_list'
    p[0] = p[1]

def p_item_call(p):
    'item : call'
    p[0] = p[1]

def p_item_empty(p):
    'item : empty'
    p[0] = p[1]

def p_call(p):
    'call : LPAREN SIMB items RPAREN'
    if DEBUG: print "Calling", p[2], "with", p[3]
    p[0] = lisp_eval(p[2], p[3])

# def p_variable_exp(p):
#     'v_exp : LPAREN VARIABLE items RPAREN'
#     p[0] = [p[2], p[3]]

# def p_let(p):
#     'exp : SIMB v_exp'
#     p[0] = [p[1], p[2]]
#
# def p_atom_let(p):
#     'atom: LET'
#     p[0] = p[1]

def p_atom_simbol(p):
    'atom : SIMB'
    # try: p[0] = variable[p[1]]
    # except LookupError:
    p[0] = p[1]

def p_atom_bool(p):
    'atom : bool'
    p[0] = p[1]

def p_atom_num(p):
    'atom : NUM'
    p[0] = p[1]

def p_atom_word(p):
    'atom : TEXT'
    p[0] = p[1]

def p_atom_empty(p):
    'atom :'
    pass

def p_true(p):
    'bool : TRUE'
    p[0] = True
    # p[0] = p[1]

def p_false(p):
    'bool : FALSE'
    p[0] = False
    # p[0] = p[2]

def p_nil(p):
    'atom : NIL'
    p[0] = None

# Error rule for syntax errors
def p_error(p):
    print "Syntax error!! ",p

# Build the parser
# Use this if you want to build the parser using SLR instead of LALR
# yacc.yacc(method="SLR")
yacc.yacc()


