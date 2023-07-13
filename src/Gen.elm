module Gen exposing (..)

import P exposing (..)
import Dict exposing (fromList, Dict)
import Json.Encode exposing (..)

fromParserResult xs = 
    let
        pick (name_, def) = case name_ of
            Just name -> Just (name, def)
            _         -> Nothing
    in
        Dict.fromList <| List.filterMap pick xs


fromParsed xs =
    case xs of
        Err e -> Err (deadEndsToString e)
        Ok lines ->
            let
                env = fromParserResult lines
                jsonize env_ ls_ =
                    case ls_ of
                        [] -> Ok []
                        ((name, GoStructDef def)::ls) ->
                            let
                                env__ =
                                    case name of
                                        Just name_ -> Dict.remove name_ env_
                                        _ -> env_
                            in
                                Result.map2 (::) (json env__ def) (jsonize env_ ls)
             in
                case jsonize env lines of
                    Err e -> Err e
                    Ok values -> Ok (String.join "\n" <| List.map (\value -> encode 4 value) values)

show : GoTypes -> String
show typ =
    case typ of
      GoBasic GoInt8 -> "int8"
      GoBasic GoInt16 -> "int16"
      GoBasic GoInt32 -> "int32"
      GoBasic GoInt64 -> "int64"
      GoBasic GoInt -> "int"
      GoBasic GoUInt8 -> "uint8"
      GoBasic GoUInt16 -> "uint16"
      GoBasic GoUInt32 -> "uint32"
      GoBasic GoUInt64 -> "uint64"
      GoBasic GoUInt -> "uint"
      GoBasic GoString -> "string"
      GoBasic GoFloat -> "float32"
      GoBasic GoDouble -> "float64"
      GoBasic GoBool -> "bool"
      GoBasic GoTime -> "time.Time" 
      _ -> "someGoType"
        
exampleValue : Dict String GoStructDef -> GoTypes -> Result String Json.Encode.Value
exampleValue env typ =
    case typ of
        GoBasic t -> Ok (simpleValue t)
        GoArrayLike n (GoBasic t) -> Ok (simpleArrayLike n t)
        GoArrayLike n t ->
            case exampleValue env t of
                Ok v -> Ok (list identity <| List.repeat n v)
                Err e -> Err e
        GoStruct (GoStructDef defs) -> json env defs
        GoTyVar t ->
            case Dict.get t env of
                Just (GoStructDef def) -> json (Dict.remove t env) def
                Nothing -> Err ("undefined or invalid: " ++ t)
        GoInterface -> Ok (object [])
        GoPointer p -> deref env 1 p
        GoMap t1 t2 ->
            case mapKey t1 t2 of
                Err e -> Err e
                Ok key ->
                    case exampleValue env t2 of
                        Err e -> Err e
                        Ok value -> Ok (object [(key, value)])

json : Dict String GoStructDef -> List GoStructLine -> Result String Json.Encode.Value
json env structfields = Result.map Json.Encode.object (json_ env structfields)

json_ : Dict String GoStructDef -> List GoStructLine -> Result String (List (String, Json.Encode.Value))
json_ env structfields =
    let
        ignored tags = List.member KeyIgnore tags
        asString tags = List.member KeyAsString tags
        rename tags =
            let
                keyRename t =
                    case t of
                        KeyRename n -> Just n
                        _ -> Nothing
            in
                List.head <| List.filterMap keyRename tags
        rename_ name tags = Maybe.withDefault name (rename tags)
        example typ asString_ =
            case typ of
                GoBasic b -> if asString_ then Ok (simpleValueAsString b) else exampleValue env typ
                _ -> exampleValue env typ
        folder structfield pairs_ =
            case pairs_ of
                Err e -> Err e
                Ok pairs ->
                    case structfield of
                        GoEmbedStruct tvar tags ->
                            if ignored tags
                                then pairs_
                                else
                                    case Dict.get tvar env of
                                        Just (GoStructDef def) ->
                                            case json_ (Dict.remove tvar env) def of
                                                Err e -> Err e
                                                Ok pairs__ ->
                                                    case rename tags of
                                                        Just name_ -> Ok ((name_, object pairs__) :: pairs)
                                                        Nothing -> Ok (pairs__ ++ pairs)
                                        Nothing -> Err ("undefined or invalid: " ++ tvar)
                        GoStructLine id typ tags ->
                            if ignored tags
                                then pairs_
                                else
                                    case example typ (asString tags) of
                                        Ok value -> Ok ((rename_ id tags, value) :: pairs)
                                        Err e -> Err e
    in
        List.foldl folder (Ok []) structfields

                
mapKey : GoTypes -> GoTypes -> Result String String
mapKey keyType valueType =
    case keyType of
        GoBasic GoString -> Ok "myKey"
        GoBasic GoInt8 -> Ok "8" 
        GoBasic GoInt16 -> Ok "16"
        GoBasic GoInt32 -> Ok "-32"
        GoBasic GoInt64 -> Ok "64"
        GoBasic GoInt -> Ok "-1024"
        GoBasic GoUInt8 -> Ok "8" 
        GoBasic GoUInt16 -> Ok "1616"
        GoBasic GoUInt32 -> Ok "323232"
        GoBasic GoUInt64 -> Ok "646464646464"
        GoBasic GoUInt -> Ok "1024"
        GoBasic GoFloat -> Ok "1.26"
        GoBasic GoDouble -> Ok "16.3"
        GoBasic GoTime -> Ok "2009-11-10T23:00:00Z"
        GoBasic GoBool -> Err ("json: unsupported type: map[bool]" ++ (show keyType))
        GoArrayLike _ _ -> Err ("json: unsupported type: map[" ++ (show keyType) ++ "]" ++ (show valueType))
        GoMap _ _ -> Err ("json: unsupported type: map[" ++ (show keyType) ++ "]" ++ (show valueType))
        GoStruct _ -> Err ("json: unsupported type: map[" ++ (show keyType) ++ "]" ++ (show valueType))
        GoInterface -> Err ("json: unsupported type: map[interface{}]" ++ (show valueType))
        GoTyVar t -> Err ("json: unsupported type: map[main." ++ t ++ "]" ++ (show valueType))
        GoPointer t -> Err ("json: unsupported type: map[*" ++ (show t) ++ "]" ++ (show valueType))

deref : Dict String GoStructDef -> Int -> GoTypes -> Result String Json.Encode.Value
deref env i typ =
    case typ of
        GoPointer p ->
            if i < 5
                then deref env (i+1) p
                else Err ("deep pointer occurrence: " ++ String.repeat i "*" ++ Debug.toString typ)
        GoTyVar t ->
            case Dict.get t env of
                Just (GoStructDef def) -> json (Dict.remove t env) def
                Nothing -> Ok Json.Encode.null
        base -> exampleValue env base


simpleArrayLike : Int -> GoSimpleTypes -> Json.Encode.Value
simpleArrayLike n typ =
    let
        taken = if n > 0 && n < 10 then (\xs -> List.take n xs) else identity
    in
        case typ of
            GoBool -> list bool <| taken [False, True, True, False, True, False, False, False, True, False]
            GoInt -> list int <| taken [1, -2, 3, -4, 5, -6, 7, -8, 9, 0]
            GoInt8 -> list int <| taken [-8, 16, 32, -64, -128, 0, 1, 2, 4, 8]
            GoInt16 -> list int <| taken [16, 17, 18, 19, -20, 21, 22, 23, -24, -25]
            GoInt32 -> list int <| taken [32, 3, -3, 4, 6, -4, 0, 2, -5, 8]
            GoInt64 -> list int <| taken [9223372036854775800, 922, 10, -9223372, 9223372036854, -1234, 56, -7, 10240, 65536]
            GoUInt -> list int <| taken [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]
            GoUInt8 -> list int <| taken [8, 16, 32, 64, 128, 0, 1, 2, 4, 8]
            GoUInt16 -> list int <| taken [16, 17, 18, 19, 20, 21, 22, 23, 24, 25]
            GoUInt32 -> list int <| taken [32, 3, 3, 4, 6, 4, 0, 2, 5, 8]
            GoUInt64 -> list int <| taken [9223372036854775800, 922, 10, 9223372, 9223372036854, 1234, 56, 7, 10240, 65536]
            GoFloat -> list float <| taken [9.5, 3.8, 1.13, 4.79, 5.023, 6.1, 2.7234, 1.9, 1.02, 5.64]
            GoDouble -> list float <| taken [8.5397342, 4.26986711133, 871.0528907127, 9.869604, 31.00627668, 10.24, 16.3, 12.6, 12.0001, 564.222]
            GoString -> list string <| taken ["rob pike", "Robert", "Pike", "gopher", "Gopher", "Go", "Golang", "goroutine", "interface{}", "struct"]
            GoTime -> list string  <| taken ["2011-01-26T19:06:43Z", "2020-07-16T14:49:50.3269159+08:00", "2011-01-26T19:01:12Z", "2011-01-26T19:14:43Z", "2009-11-10T23:00:00Z", "2018-09-22T12:42:31Z", "2020-12-29T14:58:15Z", "2006-01-02T15:04:05Z", "2020-12-29T14:58:15.229579703+08:00", "2017-12-30T11:25:30+09:00"]

simpleValue : GoSimpleTypes -> Json.Encode.Value
simpleValue typ =
    case typ of
        GoInt -> int 1
        GoInt8 -> int 127
        GoInt16 -> int (-1616)
        GoInt32 -> int 60000
        GoInt64 -> int 64
        GoUInt -> int 1
        GoUInt8 -> int 255
        GoUInt16 -> int 1616
        GoUInt32 -> int 323232
        GoUInt64 -> int 646464646464
        GoString -> string "robpike"
        GoFloat -> float 2.71828
        GoDouble -> float 3.1415926
        GoBool -> bool True
        GoTime -> string "2006-01-02T15:04:05Z"

simpleValueAsString : GoSimpleTypes -> Json.Encode.Value
simpleValueAsString typ =
    case typ of
        GoInt -> string "0"
        GoInt8 -> string "1"
        GoInt16 -> string "2"
        GoInt32 -> string "4"
        GoInt64 -> string "8"
        GoUInt -> string "0"
        GoUInt8 -> string "255"
        GoUInt16 -> string "65535"
        GoUInt32 -> string "4294967295"
        GoUInt64 -> string "18446744073709551615"
        GoString -> string "\"___\""
        GoFloat -> string "2.71828"
        GoDouble -> string "3.1415926"
        GoBool -> string "false"
        GoTime -> string "2006-01-02T15:04:05Z"
