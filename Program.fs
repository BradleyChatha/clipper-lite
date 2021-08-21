open System
open Ast
open FParsec

[<EntryPoint>]
let main argv =
    let result = runParserOnFile (many pfunc) () argv.[0] System.Text.Encoding.UTF8
    Console.WriteLine ($"{result}")

    match result with
    | Success (ast, _, _) ->
        let builder = new System.Text.StringBuilder()
        builder.Append "import std;\n" |> ignore

        for func in ast do
            let result = Transpiler.transpile (func, 0)
            for str in result do
                builder.Append str |> ignore

        Console.WriteLine (builder.ToString())
    | Failure (a, b, c) ->
        Console.WriteLine $"Error: {a}"
        ()
    0