from sexpdata import loads, dump, Symbol
import io

def simplify(expr):
    if not isinstance(expr, list) or expr == []:
        return expr
    if expr[0] == Symbol('T') and expr[2] == [Symbol('M'), 1, 0, 0, 0]:
        return simplify(expr[1])
    if expr[0] == Symbol('repeat') and expr[3] == [Symbol('M'), 1, 0, 0, 0]:
        return simplify(expr[1])
    return [simplify(x) for x in expr]

def fix(expr):
    step = simplify(expr)
    if expr == step:
        return expr
    return fix(step)


def cleanup(fnm):
    nm = fnm.split(".")[0]
    clean_fnm = str(nm) + "-cleaned.bab"
    with open(fnm, 'r') as bad, open(clean_fnm, 'w') as clean:
        bad_lines = bad.readlines()
        for l in bad_lines:
            l_to_list = list(loads(str(l)))
            fp = io.StringIO()
            cl = simplify(l_to_list)
            dump(cl, fp)
            clean.write(fp.getvalue() + "\n")


cleanup("harness/data/cogsci/dials.bab")
cleanup("harness/data/cogsci/furniture.bab")
cleanup("harness/data/cogsci/wheels.bab")
cleanup("harness/data/cogsci/nuts-bolts.bab")