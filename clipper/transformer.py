from lark import Transformer
from lark import Tree, Token


class ToLua(Transformer):
    def __init__(self):
        super().__init__()
        self.code = ""

    def root(self, root):
        for func in root:
            self.write_func(func)
        self.code += "main()"
        return self.code

    def write_func(self, func: Tree):
        ident = func.children[0]
        params = func.children[1].data == "identifier_list" and func.children[1] or None
        body = func.children[1].data == "identifier_list" and func.children[2] or func.children[1]
        self.code += "function "
        self.code += ident.__str__()

        self.code += "("
        if params:
            for param in params.children:
                self.code += param.value
                self.code += ", "
            self.code = self.code[:-2]
        self.code += ")\n"

        for exp in body.children:
            self.write_exp(exp)

        if len(func.children) == 3 and func.children[2].data == "expression":
            self.code += "return "
            self.write_exp(func.children[2])
            self.code += "\n"
        elif len(func.children) == 4 and func.children[3].data == "expression":
            self.code += "return "
            self.write_exp(func.children[3])
            self.code += "\n"
        self.code += "end\n"

    def write_exp(self, exp: Tree):
        child = exp.children[0]
        if isinstance(child, Tree):
            if exp.data == "if_expression":
                self.write_if_exp(exp)
            elif exp.data == "do_while_expression":
                self.write_dowhile_exp(exp)
            elif exp.data == "for_expression":
                self.write_for_exp(exp)
            elif exp.data == "expression_list":
                self.write_exp_list(exp)
            elif exp.data == "local_expression":
                if hasattr(exp.children[0], "data"):
                    assign_exp = exp.children[0]
                    name = assign_exp.children[0]
                    exp = assign_exp.children[1]
                    self.code += "local "
                    self.code += name
                    self.code += " = "
                    self.write_exp(exp)
                    self.code += "\n"
                else:
                    self.code += exp.children[0]
                    self.code += " = nil\n"
            elif child.data == "top_expression":
                self.write_top_exp(child)
            elif child.data == "maths_expression":
                self.write_maths_exp(child)
            elif child.data == "expression":
                self.write_exp(child)
            elif child.data == "boolean_expression":
                self.write_bool_exp(child)
            elif child.data == "func_call_expression":
                self.write_func_call_exp(child)
            elif child.data == "true":
                self.code += "true"
            elif child.data == "false":
                self.code += "false"
            elif child.data == "array_expression":
                self.write_array_exp(child)
            elif child.data == "index_expression":
                self.write_index_exp(child)
        elif isinstance(child, Token):
            self.code += child

    def write_top_exp(self, exp: Tree):
        child = exp.children[0]
        if exp.data == "if_expression":
            self.write_if_exp(exp)
        elif exp.data == "do_while_expression":
            self.write_dowhile_exp(exp)
        elif exp.data == "for_expression":
            self.write_for_exp(exp)
        elif child.data == "func_call_expression":
            self.write_func_call_exp(child)
        elif child.data == "print_expression":
            self.code += "io.write("
            self.write_exp_list(child.children[0])
            self.code += ", ' ')\n"
        elif child.data == "println_expression":
            self.code += "print("
            self.write_exp_list(child.children[0])
            self.code += ")\n"
        elif child.data == "return_expression":
            self.code += "return\n"
        elif child.data == "top_expression":
            self.write_top_exp(child)
        elif child.data == "assign_expression":
            if hasattr(child.children[1], "data"):
                name = child.children[0]
                exp = child.children[1]
                if(hasattr(name, "data")):
                    self.write_index_exp(name)
                else:
                    self.code += name
                self.code += " = "
                self.write_exp(exp)
                self.code += "\n"
            else:
                self.code += child.children[0]
                self.code += " = nil\n"
        elif child.data == "exit_expression":
            self.code += "break\n"

    def write_exp_list(self, exp: Tree):
        for e in exp.children:
            self.write_exp(e)
            self.code += ", "
        self.code = self.code[:-2]

    def write_maths_exp(self, exp: Tree):
        left = exp.children[0].children[0]
        right = exp.children[0].children[1]
        self.code += "("
        self.write_exp(left)
        if exp.children[0].data == "add_expression":
            self.code += " + "
        elif exp.children[0].data == "sub_expression":
            self.code += " - "
        elif exp.children[0].data == "mul_expression":
            self.code += " * "
        elif exp.children[0].data == "div_expression":
            self.code += " / "
        elif exp.children[0].data == "mod_expression":
            self.code += " % "
        self.write_exp(right)
        self.code += ")"

    def write_bool_exp(self, exp: Tree):
        left = exp.children[0].children[0]
        right = exp.children[0].children[1]
        self.code += "("
        self.write_exp(left)
        if exp.children[0].data == "equ_expression":
            self.code += " == "
        elif exp.children[0].data == "neq_expression":
            self.code += " ~= "
        elif exp.children[0].data == "lt_expression":
            self.code += " < "
        elif exp.children[0].data == "lte_expression":
            self.code += " <= "
        elif exp.children[0].data == "gt_expression":
            self.code += " > "
        elif exp.children[0].data == "gt_expression":
            self.code += " >= "
        self.write_exp(right)
        self.code += ")"

    def write_func_call_exp(self, exp: Tree):
        name = exp.children[0]
        args = None
        if len(exp.children) > 1:
            args = exp.children[1]

        if name == "LEN":
            self.code += "#"
            self.write_exp(args.children[0])
        elif name == "INT":
            self.code += "math.floor("
            self.write_exp_list(args)
            self.code += ")"
        elif name == "Sqrt":
            self.code += "math.sqrt("
            self.write_exp_list(args)
            self.code += ")"
        else:
            self.code += name
            self.code += "("
            if args:
                self.write_exp_list(args)
            self.code += ")\n"

    def write_if_exp(self, exp):
        ex = exp.children[0]
        body = exp.children[1]
        self.code += "if "
        self.write_exp(ex)
        self.code += " then\n"
        for e in body.children:
            self.write_top_exp(e)
        if len(exp.children) > 2:
            for branch in exp.children[2:]:
                if branch.data == "elseif_expression":
                    ex = branch.children[0]
                    body = branch.children[1]
                    self.code += "elseif "
                    self.write_exp(ex)
                    self.code += " then\n"
                    for e in body.children:
                        self.write_top_exp(e)
                else:
                    body = branch.children[0]
                    self.code += "else "
                    for e in body.children:
                        self.write_exp(e)
        self.code += "end\n"

    def write_dowhile_exp(self, exp):
        cond = exp.children[0]
        body = exp.children[1]
        self.code += "while "
        self.write_exp(cond)
        self.code += " do\n"
        for e in body.children:
            self.write_top_exp(e)
        self.code += "end\n"

    def write_for_exp(self, exp):
        assign = exp.children[0]
        to = exp.children[1]
        step = exp.children[2]
        body = exp.children[3]
        self.code += "for "
        self.code += assign.children[0] + " = "
        self.write_exp(assign.children[1])
        self.code += ", "
        self.write_exp(to)
        self.code += ", "
        self.write_exp(step)
        self.code += " do\n"
        for e in body.children:
            self.write_top_exp(e)
        self.code += "end\n"

    def write_array_exp(self, exp):
        self.code += "{"
        if len(exp.children):
            self.write_exp_list(exp.children[0])
        self.code += "}"

    def write_index_exp(self, exp):
        self.code += exp.children[0]
        self.code += "["
        self.write_exp(exp.children[1])
        self.code += "]"