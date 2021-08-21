module Ast

open System
open FParsec

type AstTypes =
    | Func          of ((AstTypes * AstTypes list) * AstTypes list) * AstTypes
    | Ident         of string
    | Number        of int64
    | True          of bool
    | False         of bool
    | Str           of string
    | Print         of AstTypes list
    | PrintLn       of AstTypes list
    | LocalAssign   of AstTypes * AstTypes
    | Assign        of AstTypes * AstTypes
    | FuncCall      of AstTypes * AstTypes list
    | Add           of AstTypes * AstTypes
    | Sub           of AstTypes * AstTypes
    | Mul           of AstTypes * AstTypes
    | Div           of AstTypes * AstTypes
    | Index         of AstTypes * AstTypes
    | Array         of AstTypes list
    | If            of AstTypes * AstTypes list
    | ElseIf        of AstTypes * AstTypes list
    | Else          of AstTypes list
    | EndIf
    | Equals        of AstTypes * AstTypes
    | NEquals       of AstTypes * AstTypes
    | LessThan      of AstTypes * AstTypes
    | LessThanE     of AstTypes * AstTypes
    | GreaterThan   of AstTypes * AstTypes
    | GreaterThanE  of AstTypes * AstTypes
    | And           of AstTypes * AstTypes
    | Or            of AstTypes * AstTypes
    | Not           of AstTypes
    | While         of AstTypes * AstTypes list
    | For           of ((AstTypes * AstTypes) * AstTypes) * AstTypes list
    | EndDo
    | Next
    | Exit
    | Void

let pident  = identifier (IdentifierOptions(Char.IsLetter, Char.IsLetter)) |>> Ident
let pnumber = pint64 |>> Number
let ptrue   = stringReturn ".T." true |>> True
let pfalse  = stringReturn ".F." false |>> False
let pbool   = ptrue <|> pfalse
let pstr    = between (pchar '"') (pchar '"') (manyChars (noneOf "\"")) |>> Str

let pexp, pexpRef = createParserForwardedToRef<AstTypes, unit>()
let ptopexp, ptopexpRef = createParserForwardedToRef<AstTypes, unit>()
let parithmetic, parithmeticRef = createParserForwardedToRef<AstTypes, unit>()
let pboolexp, pboolexpRef = createParserForwardedToRef<AstTypes, unit>()
let pcompoundboolexp, pcompoundboolexpRef = createParserForwardedToRef<AstTypes, unit>()

let pexp2 = (attempt parithmetic <|> pexp)

let pbracketexp = between (skipChar '(') (skipChar ')') pexp
let pequals     = pexp .>> spaces .>>? skipString "==" .>>. pexp |>> Equals
let pnequals    = pexp .>> spaces .>>? skipString "!=" .>>. pexp |>> NEquals
let plt         = pexp .>> spaces .>>? skipString "<" .>>. pexp |>> LessThan
let plte        = pexp .>> spaces .>>? skipString "<=" .>>. pexp |>> LessThanE
let pgt         = pexp .>> spaces .>>? skipString ">" .>>. pexp |>> GreaterThan
let pgte        = pexp .>> spaces .>>? skipString ">=" .>>. pexp |>> GreaterThanE

let pand    = pboolexp .>> spaces .>>? skipString ".AND." .>> spaces .>>. pboolexp |>> And
let por     = pboolexp .>> spaces .>>? skipString ".OR." .>> spaces .>>. pboolexp |>> Or
let pnot    = skipString ".NOT." .>>? spaces >>. pboolexp |>> Not

let padd    = pexp .>> spaces .>>? skipChar '+' .>> spaces .>>. pexp |>> Add
let psub    = pexp .>> spaces .>>? skipChar '-' .>> spaces .>>. pexp |>> Sub
let pmul    = pexp .>> spaces .>>? skipChar '*' .>> spaces .>>. pexp |>> Mul
let pdiv    = pexp .>> spaces .>>? skipChar '/' .>> spaces .>>. pexp |>> Div

let pident_list = sepBy pident (pchar ',' .>> spaces)
let pexp_list   = sepBy pexp2 (pchar ',' .>> spaces)
let parray      = between (pchar '{') (pchar '}') pexp_list |>> Array
let pprint      = skipString "??" >>. spaces1 >>. pexp_list |>> Print
let pprintln    = skipString "?" >>. spaces1 >>. pexp_list |>> PrintLn

let pfunc =
    (   spaces  >>?  (skipString "FUNCTION" <|> skipString "PROCEDURE")
    >>. spaces1 >>.  pident
    )
    .>>. 
    (
        spaces  >>.  between (skipChar '(') (skipChar ')') pident_list
    )
    .>>.
    (
        spaces1 >>.  many ptopexp
    )
    .>>.
    (
        attempt (spaces >>. skipString "RETURN" >>. spaces >>. pexp)
        <|>
        (spaces >>. stringReturn "RETURN" Void)
    )
    |>> Func

let pfunccall = 
        spaces  >>?  pident              
    .>> spaces  .>>? skipChar '(' 
                .>>. pexp_list
                .>>  skipChar ')'
    |>> FuncCall

let pindex = 
        spaces  >>?  pident
    .>> spaces  .>>? skipChar '['
    .>> spaces  .>>. pexp2
    .>> spaces  .>>  skipChar ']'
    |>> Index

let pif = 
        spaces  >>?  skipString "IF"
    >>. spaces  >>.  (pcompoundboolexp <|> pboolexp)
    .>> spaces .>>.  many ptopexp
    |>> If
let pelseif = 
        spaces  >>?  skipString "ELSEIF"
    >>. spaces  >>.  (pcompoundboolexp <|> pboolexp)
    .>> spaces  .>>. many ptopexp
    |>> ElseIf
let pelse = 
        spaces  >>?  skipString "ELSE"
    .>> spaces1 >>.  many ptopexp
    |>> Else
let pendif = spaces >>? stringReturn "ENDIF" EndIf

let penddo = spaces >>? stringReturn "ENDDO" EndDo
let pwhile =
        spaces  >>?  skipString "DO WHILE"
    .>> spaces1 >>.  pboolexp
    .>> spaces  .>>. many ptopexp
    .>> spaces  .>>  penddo
    |>> While

let pexit = spaces >>? stringReturn "EXIT" Exit

let passign = 
        notFollowedByString "RETURN"
    >>. notFollowedByString "NEXT"
    >>. notFollowedByString "ENDDO"
    >>. notFollowedByString "EXIT"
    >>. notFollowedByString "ENDIF"
    >>. notFollowedByString "ELSEIF"
    >>. notFollowedByString "ELSE"
    >>. (pindex <|> pident) .>> spaces .>> (skipString "=" <|> skipString ":=") .>> spaces .>>. pexp2 |>> Assign
let plocalassign = skipString "LOCAL" >>. spaces >>. pident .>> spaces .>> (skipString "=" <|> skipString ":=") .>> spaces .>>. pexp2 |>> LocalAssign

let pfor =
        spaces  >>?  skipString "FOR"
    .>> spaces  >>.  passign
    .>> spaces  .>>  skipString "TO"
    .>> spaces  .>>. pexp2
    .>> spaces  .>>  skipString "STEP"
    .>> spaces  .>>. pexp2
    .>> spaces  .>>. many ptopexp
    .>> spaces  .>>  skipString "NEXT"
    |>> For

pexpRef := spaces >>. choice [
    pbracketexp
    pfunccall
    pindex
    pident
    pnumber
    pbool
    pstr
    parray
] .>> spaces

parithmeticRef := spaces >>. choice [
    padd
    pdiv
    pmul
    psub
]

ptopexpRef := spaces >>. choice [
    pprint
    pprintln
    pfunccall
    pfor
    pif
    pelseif
    pelse
    pendif
    pwhile
    plocalassign
    passign
    pexit
] .>> spaces

pboolexpRef := spaces >>. choice [
    pequals
    pnequals
    plt
    plte
    pgt
    pgte
    pnot
]

pcompoundboolexpRef := spaces >>. choice [
    pand
    por
]