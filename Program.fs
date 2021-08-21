open System
open Ast
open FParsec

[<EntryPoint>]
let main argv =
    let result = run (many pfunc) @"
    FUNCTION main()
        LOCAL size     = 1000000
        LOCAL bitArray = CALL Array WITH size
        LOCAL factor   = 3
        LOCAL q        = CALL Sqrt WITH size

        FOR n = 0 TO CALL Floor WITH size / 2 STEP 1
            bitArray [n] = .T.
        NEXT

        DO WHILE factor < q
            FOR num = factor TO size STEP 1
                IF (CALL getBit WITH bitArray, num) == .T.
                    factor = num
                    EXIT
                ENDIF
            NEXT

            FOR num = factor * factor TO size STEP factor * 2
                CALL clearBit WITH bitArray, num
            NEXT

            factor = factor + 2
        ENDDO

        ? CALL countPrimes WITH bitArray
    RETURN 0

    FUNCTION countPrimes(array)
        LOCAL count = 0
        FOR i = 0 TO (CALL Len WITH array) STEP 1
            IF array [i] == .T.
                count = count + 1
            ENDIF
        NEXT
    RETURN count

    FUNCTION getBit(array, index)
    RETURN array [index / 2]

    FUNCTION clearBit(array, index)
        array [index / 2] = .F.
    RETURN
    "
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
        ()
    0