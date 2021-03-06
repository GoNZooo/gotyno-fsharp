﻿// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FParsec

type Location = { Line: uint32; Column: uint32 }

type DefinitionName = { Location: Location; Name: string }

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
    | OpenName of string
    | AppliedName of AppliedName
    | SelfReference of string

and Definition =
    | Structure of Structure
    | Union of Union
    | Import of Import
    | Enumeration of Enumeration

and Structure =
    | PlainStructure of PlainStructure
    | GenericStructure of GenericStructure

and Union =
    | PlainUnion of PlainUnion
    | GenericUnion of GenericUnion

and PlainStructure =
    { Name: string
      Fields: StructureField list }

and GenericStructure =
    { Name: string
      Fields: StructureField list
      OpenNames: string list }

and StructureField = { Name: string; Type: FieldType }

and AppliedName =
    { Name: string
      References: FieldType list }

and Array = { Type: FieldType; Length: uint64 }

and Constructor =
    { Tag: string
      Payload: FieldType option }

and PlainUnion =
    { Name: string
      Constructors: Constructor list }

and GenericUnion =
    { Name: string
      Constructors: Constructor list
      OpenNames: string list }

and Import = { Name: string; Alias: string option }

and Enumeration =
    { Name: string
      Entries: EnumerationEntry list }

and EnumerationEntry =
    { Tag: string
      Value: EnumerationValue }

and EnumerationValue =
    | String of string
    | UnsignedInteger of uint32

type ParserState =
    { NamedDefinitions: Map<string, Definition>
      CurrentOpenNames: string list
      CurrentDefinition: string option }

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

let parseBuiltinString: Parser<BuiltinType, ParserState> = pstring "String" >>% BuiltinType.String

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
    
let isLiteralStringCharacter c =
    isLetter c || isDigit c || (isAnyOf "{}[]$?&<>;/()|#^:=@!\\%") c

let parseLiteralString: Parser<FieldType, ParserState> =
    between (pchar '"') (pchar '"') (manySatisfy isLiteralStringCharacter)
    |>> LiteralString

let parseLiteralInteger: Parser<FieldType, ParserState> = puint32 |>> LiteralInteger

let getDefinition name { NamedDefinitions = namedDefinitions } = Map.tryFind name namedDefinitions

let isOpenName name { CurrentOpenNames = currentOpenNames } = List.contains name currentOpenNames

let setOpenNames names state = { state with CurrentOpenNames = names }

let isCurrentDefinition name =
    function
    | { CurrentDefinition = Some currentDefinition } -> name = currentDefinition
    | { CurrentDefinition = None } -> false

let setCurrentDefinition name state = { state with CurrentDefinition = name }

let addDefinition name definition { NamedDefinitions = namedDefinitions } =
    if Map.containsKey name namedDefinitions
    then Result.Error(sprintf "Duplicate definition with name %s found" name)
    else Result.Ok(Map.add name definition namedDefinitions)

let parseTypeReference, parseTypeReferenceImplementation = createParserForwardedToRef ()

let parseSlice, parseSliceImplementation = createParserForwardedToRef ()

let parseArray, parseArrayImplementation = createParserForwardedToRef ()

let parsePointer, parsePointerImplementation = createParserForwardedToRef ()

let parseOptional, parseOptionalImplementation = createParserForwardedToRef ()

let parseFieldType: Parser<FieldType, ParserState> =
    choice [ (parseBuiltin |>> Builtin)
             parseLiteralString
             parseLiteralInteger
             parseSlice
             parseArray
             parsePointer
             parseOptional
             (parseTypeReference |>> TypeReference) ]

do parseSliceImplementation
   := pstring "[]" >>. parseFieldType |>> Slice

do parseArrayImplementation
   := pchar '[' >>. puint64 .>> pchar ']'
      .>>. parseFieldType
      |>> fun (length, t) -> Array { Length = length; Type = t }

do parsePointerImplementation
   := pchar '*' >>. parseFieldType |>> Pointer

do parseOptionalImplementation
   := pchar '?' >>. parseFieldType |>> Optional

let parseTypeReferenceAppliedName name =
    pchar '<' >>. sepBy1 parseFieldType (pstring ", ")
    .>> pchar '>'
    |>> fun references -> AppliedName { Name = name; References = references }

do parseTypeReferenceImplementation
   := parsePascalSymbol .>>. getUserState
      >>= fun (name, state) ->
              match getDefinition name state with
              | Some definition ->
                  (choice [ parseTypeReferenceAppliedName name
                            preturn (Name definition) ])

              | None when isCurrentDefinition name state ->
                  choice [ parseTypeReferenceAppliedName name
                           preturn (SelfReference name) ]
              | None when isOpenName name state -> preturn (OpenName name)
              | None ->
                  failFatally
                      (sprintf "Definition with name %s not defined previously and is not open name in definition." name)

let parseSymbol: Parser<string, ParserState> =
    parsePascalSymbol <|> parseLowercaseSymbol

let parseStructureField: Parser<StructureField, ParserState> =
    pstring "    " >>. parseSymbol .>> pstring ": "
    .>>. parseFieldType
    |>> fun (name, fieldType) -> { Name = name; Type = fieldType }

let parseStructureFields = sepEndBy1 parseStructureField newline

let parsePlainStructure name =
    pchar '{' >>. newline >>. parseStructureFields
    .>> pchar '}'
    |>> fun fields -> PlainStructure { Name = name; Fields = fields }

let parseOpenNames = sepBy1 parsePascalSymbol (pstring ", ")

let parseGenericStructure name =
    pchar '<' >>. parseOpenNames
    >>= fun openNames ->
            updateUserState (setOpenNames openNames)
            >>% openNames
    .>> pstring ">{"
    .>> newline
    .>>. parseStructureFields
    .>> pchar '}'
    |>> fun (openNames, fields) ->
            GenericStructure
                { Name = name
                  OpenNames = openNames
                  Fields = fields }

let parseStructure =
    pstring "struct " >>. parsePascalSymbol
    .>> pchar ' '
    >>= fun name ->
            updateUserState (setCurrentDefinition (Some name))
            >>. choice [ parseGenericStructure name |>> Structure
                         parsePlainStructure name |>> Structure ]

let parseConstructor =
    pstring "    " >>. parseSymbol
    .>>. opt (pstring ": " >>. parseFieldType)
    |>> fun (tag, payload) ->
            { Constructor.Tag = tag
              Payload = payload }

let parseConstructors =
    sepEndBy1 parseConstructor newline .>> pchar '}'

let parseGenericUnion name =
    pchar '<' >>. parseOpenNames
    .>> pstring ">{"
    .>> newline
    >>= fun openNames ->
            updateUserState (setOpenNames openNames)
            >>% openNames
    .>>. parseConstructors
    |>> fun (openNames, constructors) ->
            GenericUnion
                { Name = name
                  OpenNames = openNames
                  Constructors = constructors }

let parsePlainUnion name =
    pstring "{" >>. newline >>. parseConstructors
    |>> fun constructors ->
            PlainUnion
                { Name = name
                  Constructors = constructors }

let parseUnion: Parser<Definition, ParserState> =
    pstring "union " >>. parsePascalSymbol
    >>= fun name ->
            updateUserState (setCurrentDefinition (Some name))
            >>% name
    .>> pchar ' '
    >>= fun name ->
            choice [ parseGenericUnion name |>> Union
                     parsePlainUnion name |>> Union ]

let parseAliasedImport name =
    pstring " = " >>. parseSymbol
    |>> fun alias -> { Name = name; Alias = Some alias }

let parseImport =
    pstring "import " >>. parseSymbol
    >>= fun name ->
            choice [ parseAliasedImport name
                     preturn { Name = name; Alias = None } ]

let parseEnumerationValueString =
    parseLiteralString
    >>= function
    | LiteralString s -> preturn (EnumerationValue.String s)
    | _ -> raise (Exception "Shouldn't be possible")

let parseEnumerationValueInteger =
    parseLiteralInteger
    >>= function
    | LiteralInteger i -> preturn (EnumerationValue.UnsignedInteger i)
    | _ -> raise (Exception "Shouldn't be possible")

let parseEnumerationValue =
    choice [ parseEnumerationValueString
             parseEnumerationValueInteger ]

let parseEnumerationEntry =
    pstring "    " >>. parseSymbol .>> pstring " = "
    .>>. parseEnumerationValue
    |>> fun (tag, value) -> { Tag = tag; Value = value }

let parseEnumerationEntries = sepEndBy1 parseEnumerationEntry newline

let parseEnumeration =
    pstring "enum " >>. parsePascalSymbol
    .>> pstring " {"
    .>> newline
    .>>. parseEnumerationEntries
    .>> pchar '}'
    |>> fun (name, entries) -> { Name = name; Entries = entries }

let parseDefinition: Parser<Definition, ParserState> =
    let getName =
        function
        | Structure (PlainStructure { Name = name }) -> name
        | Structure (GenericStructure { Name = name }) -> name
        | Union (PlainUnion { Name = name }) -> name
        | Union (GenericUnion { Name = name }) -> name
        | Import { Name = name } -> name
        | Enumeration { Name = name } -> name

    updateUserState (setOpenNames [] >> setCurrentDefinition None)
    >>. choice [ parseStructure
                 parseUnion
                 parseImport |>> Import
                 parseEnumeration |>> Enumeration ]
    .>> newline
    .>>. getUserState
    >>= fun (definition, state) ->
            match addDefinition (getName definition) definition state with
            | Result.Ok newNamedDefinitions ->
                setUserState
                    { state with
                          NamedDefinitions = newNamedDefinitions }
                >>% definition
            | Result.Error errorMessage -> failFatally errorMessage

let parseModule: Parser<Definition list, ParserState> = (sepBy1 parseDefinition newline) .>> eof

[<EntryPoint>]
let main _ =
    let plainStructure = """struct Recruiter {
    name: String
}

struct Node <T, U>{
    data: *T
    otherData: ?U
}

union Maybe <T>{
    Nothing
    Just: T
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

union Either <L, R>{
    Left: L
    Right: R
}

union WithMaybe <T, E>{
    WithConcrete: Maybe<String>
    WithGeneric: Maybe<T>
    WithBare: E
}

union List <T>{
    Empty
    Cons: *List<T>
}

struct LogInData {
    username: String
    password: String
}

struct UserId {
    value: String
}

struct Channel {
    name: String
    private: Boolean
}

struct Email {
    value: String
}

union Event {
    LogIn: LogInData
    LogOut: UserId
    JoinChannels: []Channel
    SetEmails: [5]Email
    Close
}

import other

import importName = aliasedName

struct S {
    type: "StructTag"
    field: U32
}

enum BackdropSize {
    w300 = "w300"
    w1280 = "w1280"
    original = "original"
}

enum IntegerEnumeration {
    Zero = 0
    One = 1
    Two = 2
}
"""

    printfn
        "%A"
        (runParserOnString
            parseModule
             { NamedDefinitions = Map.empty
               CurrentOpenNames = []
               CurrentDefinition = None }
             ""
             plainStructure)

    0 // return an integer exit code
