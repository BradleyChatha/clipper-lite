module Transpiler
open System
open Ast

let tabs level = (String(' ', level * 4))

let expectIdent ast =
    match ast with
    | Ident name -> name
    | _ -> raise (Exception($"Expected an identifier, not {ast}"))

let rec transpile (ast: AstTypes, level: int) =
    seq {
        match ast with
        | LocalAssign (name, value) ->
            yield "auto "
            yield expectIdent name
            yield " = "
            for str in (transpile (value, level)) do yield str
        | Assign (name, value) ->
            match name with
            | Ident n -> yield n
            | Index (n, i) ->
                yield expectIdent (n)
                yield "["
                for str in (transpile (i, level)) do yield str
                yield "]"
            yield " = "
            for str in (transpile (value, level)) do yield str
        | Div (left, right) ->
            for str in (transpile (left, level)) do yield str
            yield " / "
            for str in (transpile (right, level)) do yield str
        | Sub (left, right) ->
            for str in (transpile (left, level)) do yield str
            yield " - "
            for str in (transpile (right, level)) do yield str
        | Mul (left, right) ->
            for str in (transpile (left, level)) do yield str
            yield " * "
            for str in (transpile (right, level)) do yield str
        | Add (left, right) ->
            for str in (transpile (left, level)) do yield str
            yield " + "
            for str in (transpile (right, level)) do yield str
        | LessThan (left, right) ->
            for str in (transpile (left, level)) do yield str
            yield " < "
            for str in (transpile (right, level)) do yield str
        | LessThanE (left, right) ->
            for str in (transpile (left, level)) do yield str
            yield " <= "
            for str in (transpile (right, level)) do yield str
        | GreaterThan (left, right) ->
            for str in (transpile (left, level)) do yield str
            yield " > "
            for str in (transpile (right, level)) do yield str
        | GreaterThanE (left, right) ->
            for str in (transpile (left, level)) do yield str
            yield " >= "
            for str in (transpile (right, level)) do yield str
        | Equals (left, right) ->
            for str in (transpile (left, level)) do yield str
            yield " == "
            for str in (transpile (right, level)) do yield str
        | NEquals (left, right) ->
            for str in (transpile (left, level)) do yield str
            yield " != "
            for str in (transpile (right, level)) do yield str
        | True _ -> yield "true"
        | False _ -> yield "false"
        | Exit -> yield "break"
        | EndIf -> ()
        | Ident name -> yield name
        | Array _ ->
            yield "new bool[]"
        | Number num ->
            yield (string num)
            yield "L"
        | Str str ->
            yield "\""
            yield str
            yield "\""
        | Index (name, index) ->
            for str in (transpile (name, level)) do yield str
            yield "["
            for str in (transpile (index, level)) do yield str
            yield "]"
        | PrintLn args ->
            yield "writeln("
            for arg in args do
                for str in (transpile (arg, level)) do yield str
            yield ")"
        | FuncCall (name, args) ->
            let iname = expectIdent name
            if iname = "Sqrt" then
                yield "sqrt(cast(double)"
                for str in (transpile (args.[0], level)) do yield str
                yield ")"
            elif iname = "Floor" then
                yield "floor(cast(double)"
                for str in (transpile (args.[0], level)) do yield str
                yield ")"
            elif iname = "Array" then
                yield "new bool["
                for str in (transpile (args.[0], level)) do yield str
                yield "]"
            elif iname = "Len" then
                for str in (transpile (args.[0], level)) do yield str
                yield ".length"
            else
                yield iname
                yield "("
                for arg in args do
                    for str in (transpile (arg, level)) do yield str
                    yield ", "
                yield ")"
        | For (((assign, upto), step), body) ->
            let mutable assignName = ""
            match assign with
            | Assign (name, _) -> assignName <- (expectIdent name)

            yield "for(auto "
            for str in (transpile (assign, level)) do yield str
            yield "; "
            yield assignName
            yield " < "
            for str in (transpile (upto, level)) do yield str
            yield "; "
            yield assignName
            yield " += "
            for str in (transpile (step, level)) do yield str
            yield ")\n"
            yield tabs level
            yield "{\n"
            for node in body do
                yield tabs (level + 1)
                for str in (transpile (node, level+1)) do yield str
                yield ";\n"
                
            yield tabs level
            yield "}\n"
        | While (cond, body) ->
            yield "while("
            for str in (transpile (cond, level)) do yield str
            yield ")\n"
            yield tabs level
            yield "{\n"
            for node in body do
                yield tabs (level + 1)
                for str in (transpile (node, level+1)) do yield str
                yield ";\n"
            yield tabs level
            yield "}\n"
        | If (cond, body) ->
            yield "if("
            for str in (transpile (cond, level)) do yield str
            yield ")\n"
            yield tabs level
            yield "{\n"
            for node in body do
                yield tabs (level + 1)
                for str in (transpile (node, level+1)) do yield str
                yield ";\n"
            yield tabs level
            yield "}\n"
        | Else body ->
            yield "else\n"
            yield tabs level
            yield "{\n"
            for node in body do
                yield tabs (level + 1)
                for str in (transpile (node, level+1)) do yield str
                yield ";\n"
            yield tabs level
            yield "}\n"
        | Func (((name, args), body), ret) ->
            if (expectIdent name) = "main" then
                yield "void main()"
            else
                yield "\n\nauto "
                yield (expectIdent name)
                yield "("
                for i in 1..args.Length do
                    yield "Arg"
                    yield (string i)
                    yield ", " 
                yield ")("
                for i in 1..args.Length do
                    yield "Arg"
                    yield (string i)
                    yield " "
                    yield (expectIdent args.[i-1])
                    yield ", "
                yield ")"
            yield "\n{\n"
            for node in body do
                yield tabs (level+1)
                let r = transpile (node, level+1)
                for str in r do yield str
                yield ";\n"
            yield tabs 1
            yield "return "
            if (expectIdent name) <> "main" then
                match ret with
                | Void -> ()
                | somethingElse -> for str in (transpile (somethingElse, level + 1)) do yield str
            yield ";\n"
            yield "\n}"
    }