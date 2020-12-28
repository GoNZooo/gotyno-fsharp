// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FParsec

type BuiltinType =
    | String
    | U8
    | U16
    | U32
    | U64
    | U128
    | I8
    | I16
    | I32
    | I64
    | I128
    | F32
    | F64
    | F128
    | Boolean

type FieldType =
    | Builtin of BuiltinType
    | LiteralString of string
    | LiteralInteger of uint32

type StructureField = { Name: string; Type: FieldType }

type PlainStructure =
    { Name: String
      Fields: list<StructureField> }

type Structure = PlainStructure of PlainStructure

type Definition = Structure of Structure

let parseBuiltinString: Parser<BuiltinType, unit> = pstring "String" >>% String

let parseBuiltinBoolean: Parser<BuiltinType, unit> = pstring "Boolean" >>% Boolean

let parseBuiltinInteger: Parser<BuiltinType, unit> =
    (pstring "I8" >>% I8)
    <|> (pstring "I16" >>% I16)
    <|> (pstring "I32" >>% I32)
    <|> (pstring "I64" >>% I64)
    <|> (pstring "I128" >>% I128)
    <|> (pstring "U8" >>% U8)
    <|> (pstring "U16" >>% U16)
    <|> (pstring "U32" >>% U32)
    <|> (pstring "U64" >>% U64)
    <|> (pstring "U128" >>% U128)

let parseBuiltinFloat: Parser<BuiltinType, unit> =
    (pstring "F32" >>% F32)
    <|> (pstring "F64" >>% F64)
    <|> (pstring "F128" >>% F128)

let parseBuiltin: Parser<BuiltinType, _> =
    parseBuiltinString
    <|> parseBuiltinBoolean
    <|> parseBuiltinInteger
    <|> parseBuiltinFloat

let parseLiteralString: Parser<FieldType, unit> =
    between (pchar '"') (pchar '"') (manyChars anyChar)
    |>> LiteralString

let parseLiteralInteger: Parser<FieldType, unit> = puint32 |>> LiteralInteger

let parseFieldType: Parser<FieldType, unit> =
    (parseBuiltin |>> Builtin)
    <|> parseLiteralString
    <|> parseLiteralInteger

[<EntryPoint>]
let main _ =
    printfn "%A" (run parseBuiltin "Boolea")
    0 // return an integer exit code
