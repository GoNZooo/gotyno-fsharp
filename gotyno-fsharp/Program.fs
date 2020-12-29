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
    | Slice of FieldType
    | Array of Array
    | Optional of FieldType
    | Pointer of FieldType
    | TypeReference of TypeReference

and TypeReference =
    | Name of Definition
    | AppliedName of AppliedName

and Definition = Structure of Structure

and Structure = PlainStructure of PlainStructure

and PlainStructure =
    { Name: String
      Fields: StructureField list }

and StructureField = { Name: string; Type: FieldType }

and AppliedName =
    { Name: string
      References: FieldType list }

and Array = { Type: FieldType; Length: uint64 }

type Location = { Line: uint32; Column: uint32 }

type DefinitionName = { Location: Location; Name: string }

type ParserState =
    { NamedDefinitions: Map<string, Definition> }

let isValidSymbolCharacter c =
    Char.IsLetterOrDigit(c)
    || Array.contains c [| '_' |]

let parseValidSymbolCharacter: Parser<char, ParserState> = satisfy isValidSymbolCharacter

let parsePascalSymbol: Parser<string, ParserState> =
    asciiUpper
    .>>. manyChars parseValidSymbolCharacter
    |>> fun (first, rest) -> string (first) + rest

let parseLowercaseSymbol: Parser<string, ParserState> =
    asciiLower
    .>>. manyChars parseValidSymbolCharacter
    |>> fun (first, rest) -> string (first) + rest

let parseBuiltinString: Parser<BuiltinType, ParserState> = pstring "String" >>% String

let parseBuiltinBoolean: Parser<BuiltinType, ParserState> = pstring "Boolean" >>% Boolean

let parseBuiltinInteger: Parser<BuiltinType, ParserState> =
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

let parseBuiltinFloat: Parser<BuiltinType, ParserState> =
    (pstring "F32" >>% F32)
    <|> (pstring "F64" >>% F64)
    <|> (pstring "F128" >>% F128)

let parseBuiltin: Parser<BuiltinType, ParserState> =
    parseBuiltinString
    <|> parseBuiltinBoolean
    <|> parseBuiltinInteger
    <|> parseBuiltinFloat

let parseLiteralString: Parser<FieldType, ParserState> =
    between (pchar '"') (pchar '"') (manyChars anyChar)
    |>> LiteralString

let parseLiteralInteger: Parser<FieldType, ParserState> = puint32 |>> LiteralInteger

let getDefinition name { NamedDefinitions = namedDefinitions } = Map.tryFind name namedDefinitions

let addDefinition name definition { NamedDefinitions = namedDefinitions } =
    if Map.containsKey name namedDefinitions
    then Result.Error(sprintf "Duplicate definition with name %s found" name)
    else Result.Ok(Map.add name definition namedDefinitions)

let parseTypeReference, parseTypeReferenceImplementation = createParserForwardedToRef ()

let parseSlice, parseSliceImplementation = createParserForwardedToRef ()

let parseArray, parseArrayImplementation = createParserForwardedToRef ()

let rec parseFieldType: Parser<FieldType, ParserState> =
    choice [ (parseBuiltin |>> Builtin)
             parseLiteralString
             parseLiteralInteger
             parseSlice
             parseArray
             parseTypeReference ]

do parseSliceImplementation
   := pstring "[]" >>. parseFieldType |>> Slice

do parseArrayImplementation
   := pchar '[' >>. puint64 .>> pchar ']'
      .>>. parseFieldType
      |>> fun (length, t) -> Array { Length = length; Type = t }

let parseTypeReferenceAppliedName name =
    pchar '<' >>. sepBy1 parseFieldType (pstring ", ")
    .>> pchar '>'
    |>> fun references -> TypeReference(AppliedName { Name = name; References = references })

do parseTypeReferenceImplementation
   := parsePascalSymbol .>>. getUserState
      >>= fun (name, state) ->
              match getDefinition name state with
              | Some definition ->
                  (choice [ parseTypeReferenceAppliedName name
                            preturn (TypeReference(Name definition)) ])

              | None -> failFatally (sprintf "Definition with name %s not defined previously." name)

let parseSymbol: Parser<string, ParserState> =
    parsePascalSymbol <|> parseLowercaseSymbol

let parseStructureField: Parser<StructureField, ParserState> =
    parseSymbol .>> pstring ": " .>>. parseFieldType
    .>> pchar '\n'
    |>> fun (name, fieldType) -> { Name = name; Type = fieldType }

let parsePlainStructure: Parser<Definition, ParserState> =
    pstring "struct " >>. parsePascalSymbol
    .>> pstring " {"
    .>> pchar '\n'
    .>>. many1 (pstring "    " >>. parseStructureField)
    .>> pchar '}'
    |>> fun (name, fields) -> Structure(PlainStructure { Name = name; Fields = fields })

let parseDefinition: Parser<Definition, ParserState> =
    let getName =
        function
        | Structure (PlainStructure { Name = name }) -> name

    choice [ parsePlainStructure ] .>>. getUserState
    >>= fun (definition, state) ->
            match addDefinition (getName definition) definition state with
            | Result.Ok newNamedDefinitions ->
                setUserState
                    { state with
                          NamedDefinitions = newNamedDefinitions }
                >>% definition
            | Result.Error errorMessage -> failFatally errorMessage

let parseModule: Parser<Definition list, ParserState> =
    sepEndBy1 parseDefinition (skipMany1 (pchar '\n'))
    .>> eof

[<EntryPoint>]
let main _ =
    let plainStructure = """struct Recruiter {
    name: String
}

struct Person {
    name: String
    age: U8
    efficiency: F32
    on_vacation: Boolean
    hobbies: []String
    last_fifteen_comments: [15]String
    recruiter: Recruiter
    spouse: Maybe<Person>
}
"""

    //    hobbies: []String
//    last_fifteen_comments: [15]String
//    recruiter: Recruiter
//    spouse: Maybe<Person>
//}
    printfn "%A" (runParserOnString parseModule { NamedDefinitions = Map.empty } "" plainStructure)

    0 // return an integer exit code
