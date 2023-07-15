module Gen exposing (..)

import P exposing (..)
import Dict exposing (Dict)
import Json.Encode exposing (..)
import Parser exposing (DeadEnd)
import Random
import Random.Char
import Random.Dict
import Random.Float
import Random.Int
import Random.Extra
import Random.List
import Random.String

buildEnv : List (Maybe String, GoStructDef) -> Result String (Dict String GoStructDef)
buildEnv xs =
    let
        folder (name_, def) dict_ =
            case dict_ of
                Ok dict ->
                    case name_ of
                        Just name ->
                            if Dict.member name dict
                                then Err <| name ++ " redeclared in this block"
                                else Ok  <| Dict.insert name def dict
                        Nothing -> dict_
                err -> err
    in
        List.foldl folder (Ok Dict.empty) xs

fromParsed : Result (List DeadEnd) (List (Maybe String, GoStructDef)) -> Random.Generator (Result String String)
fromParsed result =
    case result of
        Err e -> Random.constant <| Err <| deadEndsToString e
        Ok lines ->
            let
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
                case buildEnv lines of
                    Ok env ->
                        case jsonize env lines of
                            Err e -> Random.constant <| Err e
                            Ok values -> Random.map (Ok
                                << String.join "\n"
                                << List.map (\value -> encode 4 value))
                                <| Random.Extra.sequence values
                    Err e -> Random.constant (Err e)

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
        GoArrayLike Nothing t -> "[]" ++ show t
        GoArrayLike (Just n) t -> "[" ++ String.fromInt n ++ "]" ++ show t
        GoMap t1 t2 -> "map[" ++ show t1 ++ "]" ++ show t2
        GoStruct _ -> "struct{...}"
        GoInterface -> "interface{}"
        GoTyVar t -> "main." ++ t
        GoPointer t -> "*" ++ show t
        
exampleValue : Dict String GoStructDef -> GoTypes -> Result String (Random.Generator Json.Encode.Value)
exampleValue env typ =
    case typ of
        GoBasic t -> Ok (simpleValue t)
        GoArrayLike n (GoBasic t) -> Ok
            <| simpleArrayLike (Maybe.withDefault 3 n)
            <| t
        GoArrayLike n t ->
            case exampleValue env t of
                Ok vg -> Ok
                    <| Random.map (\v -> list identity
                        <| List.repeat (Maybe.withDefault 3 n)
                        <| v)
                    <| vg
                Err e -> Err e
        GoStruct (GoStructDef defs) -> json env defs
        GoTyVar t ->
            case Dict.get t env of
                Just (GoStructDef def) -> json (Dict.remove t env) def
                Nothing -> Err <| "undefined or invalid: " ++ t
        GoInterface -> Ok <| Random.constant <| object []
        GoPointer p ->
            case deref 1 p of
                Ok t -> exampleValue env t
                Err e -> Err e
        GoMap t1 t2 ->
            case mapKey t1 t2 of
                Err e -> Err e
                Ok keyGenerator ->
                    case exampleValue env t2 of
                        Err e -> Err e
                        Ok valueGenerator -> Ok
                            <| Random.map (\d -> Json.Encode.dict identity identity d)
                            <| Random.Dict.rangeLengthDict 3 5 keyGenerator valueGenerator

json : Dict String GoStructDef -> List GoStructLine -> Result String (Random.Generator Json.Encode.Value)
json env structfields = Result.map (\v -> Random.map Json.Encode.object v) <| json_ env structfields

json_ : Dict String GoStructDef -> List GoStructLine -> Result String (Random.Generator (List (String, Json.Encode.Value)))
json_ env structfields =
    let
        ignored tags = List.member KeyIgnore tags
        asString tags = List.member KeyAsString tags
        notExported id = Maybe.withDefault True << Maybe.map (\(c, _) -> not <| Char.isUpper <| c) <| String.uncons <| id
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
                GoPointer p ->
                -- json tag "string" expecting: string, floating point, integer, or boolean types
                -- other types ignore it, just proceed as usual
                -- which I seemed to misunderstand in the original Haskell version
                -- "Pointer values encode as the value pointed to."
                    case deref 1 p of
                        Ok (GoTyVar t) ->
                            case Dict.get t env of
                                Just (GoStructDef def) -> json (Dict.remove t env) def
                                Nothing -> Ok <| Random.constant <| Json.Encode.null -- special case
                        Ok t -> exampleValue env t
                        Err e -> Err e
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
                                                        Just name_ -> Ok <| Random.map2 (\p0 p1 -> (name_, object p1) :: p0) pairs pairs__
                                                        Nothing    -> Ok <| Random.map2 (\p0 p1 -> p0 ++ p1) pairs__ pairs
                                        Nothing -> Err ("undefined or invalid: " ++ tvar)
                        GoStructLine id typ tags ->
                            if ignored tags
                                then pairs_
                                else
                                    if notExported id
                                        -- XXX: what to do with an JSON empty tag?
                                        then
                                            if not (List.isEmpty tags)
                                                then Err <| "struct field " ++ id ++ " has json tag but is not exported"
                                                else pairs_
                                    else
                                        case example typ (asString tags) of
                                            Ok generator -> Ok <| Random.map2 (\p0 value -> (rename_ id tags, value) :: p0) pairs generator
                                            Err e -> Err e
    in
        List.foldl folder (Ok (Random.constant [])) structfields

                
mapKey : GoTypes -> GoTypes -> Result String (Random.Generator String)
mapKey keyType valueType =
    case keyType of
        GoBasic GoString -> Ok <| Random.String.rangeLengthString 1 5 Random.Char.lowerCaseLatin
        GoBasic GoInt8 -> Ok <| Random.map String.fromInt goInt8
        GoBasic GoInt16 -> Ok <| Random.map String.fromInt goInt16
        GoBasic GoInt32 -> Ok <| Random.map String.fromInt goInt32
        GoBasic GoInt64 -> Ok <| Random.map String.fromInt goInt64
        GoBasic GoInt -> Ok <| Random.map String.fromInt goInt
        GoBasic GoUInt8 -> Ok <| Random.map String.fromInt goUInt8
        GoBasic GoUInt16 -> Ok <| Random.map String.fromInt goUInt16
        GoBasic GoUInt32 -> Ok <| Random.map String.fromInt goUInt32
        GoBasic GoUInt64 -> Ok <| Random.map String.fromInt goUInt64
        GoBasic GoUInt -> Ok <| Random.map String.fromInt goUInt
        GoBasic GoFloat -> Ok <| Random.map String.fromFloat Random.Float.anyFloat
        GoBasic GoDouble -> Ok <| Random.map String.fromFloat Random.Float.anyFloat
        GoBasic GoTime -> Ok <| Random.constant "2009-11-10T23:00:00Z"
        GoBasic GoBool -> Err <| "json: unsupported type: map[bool]" ++ show valueType
        GoArrayLike _ _ -> Err <| "json: unsupported type: map[" ++ show keyType ++ "]" ++ show valueType
        GoMap _ _ -> Err <| "json: unsupported type: map[" ++ show keyType ++ "]" ++ show valueType
        GoStruct _ -> Err <| "json: unsupported type: map[" ++ show keyType ++ "]" ++ show valueType
        GoInterface -> Err <| "json: unsupported type: map[interface{}]" ++ show valueType
        GoTyVar t -> Err <| "json: unsupported type: map[main." ++ t ++ "]" ++ show valueType
        GoPointer t -> Err <| "json: unsupported type: map[*" ++ show t ++ "]" ++ show valueType

deref : Int -> GoTypes -> Result String GoTypes
deref i typ =
    case typ of
        GoPointer p ->
            if i < 5
                then deref (i+1) p
                else Err <| "deep pointer occurrence: " ++ String.repeat i "*" ++ show typ
        base -> Ok base

simpleArrayLike : Int -> GoSimpleTypes -> Random.Generator Json.Encode.Value
simpleArrayLike n typ =
    let
        value f g = Random.map (\xs -> list f xs) g 
        randomTake f xs = Random.map (\ys -> list f <| List.take n ys) <| Random.List.shuffle xs
    in
        case typ of
            GoBool -> value bool <| Random.list n Random.Extra.bool
            GoInt -> value int <| Random.list n Random.Int.anyInt
            GoInt8 -> value int <| Random.list n goInt8
            GoInt16 -> value int <| Random.list n goInt16
            GoInt32 -> value int <| Random.list n goInt32
            GoInt64 -> value int <| Random.list n Random.Int.anyInt
            GoUInt -> value int <| Random.list n Random.Int.positiveInt
            GoUInt8 -> value int <| Random.list n goUInt8
            GoUInt16 -> value int <| Random.list n goUInt16
            GoUInt32 -> value int <| Random.list n goUInt32
            GoUInt64 -> value int <| Random.list n goUInt64
            GoFloat -> value float <| Random.list n Random.Float.anyFloat
            GoDouble -> value float <| Random.list n Random.Float.anyFloat
            GoString -> value string <| Random.list n <| Random.String.rangeLengthString 1 8 Random.Char.english
            GoTime -> randomTake string ["2011-01-26T19:06:43Z", "2020-07-16T14:49:50.3269159+08:00", "2011-01-26T19:01:12Z", "2011-01-26T19:14:43Z", "2009-11-10T23:00:00Z", "2018-09-22T12:42:31Z", "2020-12-29T14:58:15Z", "2006-01-02T15:04:05Z", "2020-12-29T14:58:15.229579703+08:00", "2017-12-30T11:25:30+09:00"]

simpleValue : GoSimpleTypes -> Random.Generator Json.Encode.Value
simpleValue typ =
    case typ of
        GoInt -> Random.map int Random.Int.anyInt
        GoInt8 -> Random.map int <| Random.int -128 127
        GoInt16 -> Random.map int <| Random.int -32768 32767
        GoInt32 -> Random.map int <| Random.int -323232 323232
        GoInt64 -> Random.map int Random.Int.anyInt
        GoUInt -> Random.map int Random.Int.positiveInt
        GoUInt8 -> Random.map int goUInt8
        GoUInt16 -> Random.map int goUInt16
        GoUInt32 -> Random.map int goUInt32
        GoUInt64 -> Random.map int goUInt64
        GoString -> Random.map string goString
        GoFloat -> Random.map float Random.Float.anyFloat
        GoDouble -> Random.map float Random.Float.anyFloat
        GoBool -> Random.map bool Random.Extra.bool
        GoTime -> Random.constant <| string "2006-01-02T15:04:05Z"

simpleValueAsString : GoSimpleTypes -> Random.Generator Json.Encode.Value
simpleValueAsString typ =
    case typ of
        GoInt ->  intString goInt
        GoInt8 -> intString goInt8
        GoInt16 -> intString goInt16
        GoInt32 -> intString goInt32
        GoInt64 -> intString goInt64
        GoUInt -> intString goUInt
        GoUInt8 -> intString goUInt8
        GoUInt16 -> intString goUInt16 
        GoUInt32 -> intString goUInt32
        GoUInt64 -> intString goUInt64  
        GoString -> Random.constant <| string "\"robpike\""
        GoFloat -> floatString <| Random.float 0 1
        GoDouble -> floatString <| Random.float 0 1
        GoBool -> Random.map (\b -> if b then string "true" else string "false") Random.Extra.bool
        GoTime -> Random.constant <| string "2006-01-02T15:04:05Z"

goInt8 : Random.Generator Int
goInt8 = Random.int -128 127

goInt16 : Random.Generator Int
goInt16 = Random.int -32768 32767

goInt64 : Random.Generator Int
goInt64 = Random.Int.anyInt

goInt : Random.Generator Int
goInt = Random.Int.anyInt

goInt32 : Random.Generator Int
goInt32 = Random.int -323232 323232

goUInt : Random.Generator Int
goUInt = Random.Int.positiveInt

goUInt8 : Random.Generator Int
goUInt8 = Random.int 0 255

goUInt16 : Random.Generator Int
goUInt16 = Random.int 0 65535

goUInt32 : Random.Generator Int
goUInt32 = Random.int 0 323232

goUInt64 : Random.Generator Int
goUInt64 = Random.int 0 646464646464

stringValue : (a -> String) -> Random.Generator a -> Random.Generator Value
stringValue f g = Random.map (string << f) g

intString : Random.Generator Int -> Random.Generator Value
intString = stringValue String.fromInt

floatString : Random.Generator Float -> Random.Generator Value
floatString = stringValue String.fromFloat

goString : Random.Generator String
goString =
    Random.map (\s -> Maybe.withDefault "nil" s)
    <| Random.Extra.sample
    <| ["rob pike", "Robert", "Pike", "gopher", "Gopher", "Go", "Golang", "goroutine", "interface{}", "struct"]
