from lark import Lark

clipper_parser = Lark(r"""
    root: func+
    func: "FUNCTION" identifier "(" identifier_list? ")" expressions "RETURN" expression?

    ?identifier: /(?!RETURN)(?!NEXT)(?!CALL)(?!WITH)(?!\()(?!\))[\w_\.]+/
    identifier_list: identifier ", " identifier_list+
                   | identifier

    expressions:            (if_expression | do_while_expression | for_expression | local_expression | expression)*
    expression:             "(" expression+ ")"
                        |   func_call_expression
                        |   top_expression
                        |   boolean_expression
                        |   maths_expression
                        |   array_expression
                        |   index_expression
                        |   ESCAPED_STRING
                        |   NUMBER
                        |   identifier
    top_expression:         assign_expression
                        |   print_expression
                        |   println_expression
                        |   exit_expression
    maths_expression:       div_expression
                        |   mul_expression
                        |   add_expression
                        |   sub_expression
                        |   mod_expression
    boolean_expression:     not_expression
                        |   lt_expression
                        |   lte_expression
                        |   gte_expression
                        |   gt_expression
                        |   and_expression
                        |   or_expression
                        |   equ_expression
                        |   neq_expression
                        |   ".T."           -> true
                        |   ".F."           -> false
    if_expression:          "IF" expression expressions elseif_expression? elseif_expression? elseif_expression? elseif_expression? else_expression? "ENDIF"
    elseif_expression:      "ELSEIF" expression expressions
    else_expression:        "ELSE" expressions
    do_while_expression:    "DO" "WHILE" expression expressions "ENDDO"
    for_expression:         "FOR" assign_expression "TO" expression "STEP" expression expressions "NEXT"
    mul_expression:         expression "*" expression
    add_expression:         expression "+" expression
    sub_expression:         expression "-" expression
    div_expression:         expression "/" expression
    mod_expression:         expression "%" expression
    and_expression:         expression ".AND." expression
    or_expression:          expression ".OR." expression
    not_expression:         ".NOT." expression
    equ_expression:         expression "==" expression | expression "=" expression
    neq_expression:         expression "!=" expression
    lt_expression :         expression "<" expression
    gt_expression:          expression ">" expression
    lte_expression :        expression "<=" expression
    gte_expression:         expression ">=" expression
    println_expression:     "?" expression_list
    print_expression:       "??" expression_list
    assign_expression:      identifier ":=" expression 
                            | identifier "=" expression 
                            | index_expression "=" expression 
                            | index_expression ":=" expression
    local_expression:       "LOCAL" identifier | "LOCAL" assign_expression
    array_expression:       "{" expression_list? "}"
    index_expression:       identifier "[" expression "]"
    func_call_expression:   "CALL" identifier "WITH" expression_list
    exit_expression:        "EXIT"
              
    expression_list: expression ", " expression_list+
                   | expression

    %import common.ESCAPED_STRING
    %import common.NUMBER
    %ignore /[ \t\r\f\n]+/
""", start='root')