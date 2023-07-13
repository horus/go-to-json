module P exposing (..)

import Parser exposing (..)

type GoStructDef = GoStructDef (List GoStructLine)
type GoStructLine = GoStructLine String GoTypes (List JSONTags) | GoEmbedStruct String (List JSONTags)
type JSONTags = KeyRename String | KeyOmitEmpty | KeyIgnore | KeyAsString
type GoSimpleTypes = GoInt8 | GoInt16 | GoInt32 | GoInt64 | GoInt | GoUInt8 | GoUInt16 | GoUInt32 | GoUInt64 | GoUInt | GoString | GoDouble | GoFloat | GoBool | GoTime
type GoTypes = GoBasic GoSimpleTypes | GoArrayLike Int GoTypes | GoMap GoTypes GoTypes | GoStruct GoStructDef | GoInterface | GoTyVar String | GoPointer GoTypes

goStructs : Parser (List (Maybe String, GoStructDef))
goStructs = lineEnding |> andThen (\_ -> goStructs_ |. end)

goStructs_ : Parser (List (Maybe String, GoStructDef))
goStructs_ = loop [] (\structs -> oneOf [
    succeed (\struct -> Loop (struct :: structs))
        |. lineEnding
        |= oneOf [ goTypeDefStruct, succeed (\s -> (Nothing, s)) |= goStructDef ]
        |. lineEnding,
    succeed () |> map (\_ -> Done (List.reverse structs))
    ])

goTypeDefStruct : Parser (Maybe String, GoStructDef)
goTypeDefStruct = succeed (\id def -> (Just id, def))
    |. keyword "type"
    |. spaces_
    |= goIdent
    |. spaces_
    |= goStructDef

goIdent : Parser String
goIdent =
    chompWhile Char.isAlphaNum
        |> getChompedString
        |> andThen (\s -> if s /= "" then succeed s else problem "empty identifier")

goTypeLit : Parser GoTypes
goTypeLit =
    let
        goBasic = succeed GoBasic |= goBasicTypeLit
        goPointer = succeed GoPointer |. symbol "*" |= lazy (\_ -> goTypeLit)
        goInterface = succeed GoInterface |. oneOf [ keyword "interface" |. symbol "{" |. symbol "}", keyword "any" ]
        goArrayLike = succeed GoArrayLike |. symbol "[" |= oneOf [ goSlice, goArray ] |= lazy (\_ -> goTypeLit)
        goSlice = succeed 0 |. symbol "]"
        goArray = succeed identity |= int |. symbol "]"
        goMap = succeed GoMap |. keyword "map" |. symbol "[" |= lazy (\_ -> goTypeLit) |. symbol "]" |= lazy (\_ -> goTypeLit)
        goStruct = succeed GoStruct |= goStructDef
        goTyVar = succeed GoTyVar |= goIdent
    in
        oneOf [ goPointer, goInterface, goBasic, goArrayLike, goMap, goStruct, goTyVar ]

-- goStructDef = succeed GoStructDef
--     |. keyword "struct"
--     |. spaces
--     |. symbol "{"
--     |= loop [] (\lines -> oneOf [
--         succeed (\line -> Loop (line :: lines))
--             |. lineEnding
--             |= goStructLine
--             |. lineEnding,
--         succeed () |> map (\_ -> Done (List.reverse lines))
--         ])
--     |. spaces
--     |. symbol "}"

goStructDef = succeed GoStructDef
    |. keyword "struct"
    |. spaces
    |= sequence { start = "{"
                , separator = ""
                , end = "}"
                , spaces = lineEnding
                , item = goStructLine
                , trailing = Optional
                }

goBasicTypeLit : Parser GoSimpleTypes
goBasicTypeLit =
 oneOf 
    [ succeed GoInt8   |. keyword "int8",
      succeed GoInt16  |. keyword "int16",
      succeed GoInt32  |. keyword "int32",
      succeed GoInt64  |. keyword "int64",
      succeed GoInt    |. keyword "int",
      succeed GoUInt8  |. keyword "uint8",
      succeed GoUInt16 |. keyword "uint16",
      succeed GoUInt32 |. keyword "uint32",
      succeed GoUInt64 |. keyword "uint64",
      succeed GoUInt   |. keyword "uint",
      succeed GoString |. keyword "string",
      succeed GoFloat  |. keyword "float32",
      succeed GoDouble |. keyword "float64",
      succeed GoBool   |. keyword "bool",
      succeed GoTime   |. keyword "time.Time"
    ]

goStructJsonTag : String -> List JSONTags
goStructJsonTag tag_ =
    let
      rename x xs = if x == "" then xs else KeyRename x :: xs
      toTag t =
        case t of
            "omitempty" -> Just KeyOmitEmpty
            "string"    -> Just KeyAsString
            _           -> Nothing
      tag = String.dropRight 1 (String.dropLeft 6 tag_) -- json:"..."
    in
        case tag of
            ""  -> []
            "-" -> [KeyIgnore]
            ts  -> case String.split "," ts of
                    [] -> []
                    (x :: xs) -> rename x (List.filterMap toTag xs)

goStructLine : Parser GoStructLine
goStructLine = 
    let goStructTags = oneOf [
            succeed identity
                |. symbol "`"
                |= (getChompedString <| chompUntil "`")
                |. symbol "`"
                |> andThen parseTags,
            succeed []
            ]
        parseTags tagline =
            -- https://pkg.go.dev/reflect#StructTag
            let
                tags = String.words tagline
                isJsonTag tag = String.startsWith "json:\"" tag
            in
                case List.filter isJsonTag tags of
                    [] -> succeed []
                    (tag :: _) -> succeed (goStructJsonTag tag)
    in
        succeed identity
        |. lineEnding
        |= goIdent
        |. spaces_
        |> andThen (\i -> oneOf [
                succeed (GoStructLine i)
                |= goTypeLit
                |. spaces_
                |= goStructTags,
                succeed (GoEmbedStruct i)
                |. spaces_
                |= goStructTags
            ])

spaces_ : Parser ()
spaces_ = chompWhile (\c -> c == ' ' || c == '\t')

lineEnding : Parser ()
lineEnding = loop 0 <| ifProgress <| oneOf
    [ lineComment "//"
    , multiComment "/*" "*/" NotNestable
    , chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r') -- "Use spaces only if you must :-P"
    ]

ifProgress : Parser a -> Int -> Parser (Step Int ())
ifProgress parser offset = succeed identity
    |. parser |= getOffset |> map (\newOffset -> if offset == newOffset then Done () else Loop newOffset)

deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
  String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))

deadEndToString : DeadEnd -> String
deadEndToString deadend =
  problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col

problemToString : Problem -> String
problemToString p =
  case p of
   Expecting s -> "expecting '" ++ s ++ "'"
   ExpectingInt -> "expecting int"
   ExpectingHex -> "expecting hex"
   ExpectingOctal -> "expecting octal"
   ExpectingBinary -> "expecting binary"
   ExpectingFloat -> "expecting float"
   ExpectingNumber -> "expecting number"
   ExpectingVariable -> "expecting variable"
   ExpectingSymbol s -> "expecting symbol '" ++ s ++ "'"
   ExpectingKeyword s -> "expecting keyword '" ++ s ++ "'"
   ExpectingEnd -> "expecting end"
   UnexpectedChar -> "unexpected char"
   Problem s -> "problem: " ++ s
   BadRepeat -> "bad repeat"
