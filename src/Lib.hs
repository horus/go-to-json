{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lib where

import Control.Monad (foldM)
import Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Lazy (toStrict)
import Data.Char (isAlphaNum, isUpper)
import Data.Foldable (foldlM)
import Data.Functor (($>))
import qualified Data.HashMap.Strict as HM
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack)
import qualified Data.Vector as V
import Data.Void (Void)
import Numeric (showInt)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

getJson :: String -> ByteString
getJson "" = ""
getJson s = either C8.pack id result
  where
    result =
      case parse goStructs "" s of
        Left err -> Left $ errorBundlePretty err
        Right defs -> jsonize defs

newtype GoStructDef = GoStructDef [GoStructLine]

instance Show GoStructDef where
  show (GoStructDef []) = "struct {}"
  show (GoStructDef defs) = "struct {\n" ++ concatMap (\line -> "    " ++ show line ++ "\n") defs ++ "}"

data JSONTags = KeyRename String | KeyOmitEmpty | KeyIgnore | KeyAsString | KeyUnknown String deriving (Show, Eq)

data GoStructLine = GoStructLine String GoTypes [JSONTags] | GoEmbedStruct String [JSONTags] -- Id type tags

instance Show GoStructLine where
  show (GoStructLine k typ _) = k ++ " " ++ show typ
  show (GoEmbedStruct t _) = t

goStructs :: Parser [(Maybe String, GoStructDef)]
goStructs = some (typedef <|> fmap (Nothing,) goStruct)

typedef = liftA2 (,) name def
  where
    name = (string "type" >> hspace1) *> fmap Just goIdent
    def = hspace1 *> goStruct

goStruct :: Parser GoStructDef
goStruct =
  string "struct"
    *> hspace
    *> between
      (symbol "{")
      (symbol "}")
      (GoStructDef <$> goStructLine `sepEndBy` sc)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol sc

data GoSimpleTypes = GoInt8 | GoInt16 | GoInt32 | GoInt64 | GoInt | GoUInt8 | GoUInt16 | GoUInt32 | GoUInt64 | GoUInt | GoString | GoDouble | GoFloat | GoBool | GoTime deriving (Eq)

instance Show GoSimpleTypes where
  show GoInt = "int"
  show GoInt8 = "int8"
  show GoInt16 = "int16"
  show GoInt32 = "int32"
  show GoInt64 = "int64"
  show GoUInt = "uint"
  show GoUInt8 = "uint8"
  show GoUInt16 = "uint16"
  show GoUInt32 = "uint32"
  show GoUInt64 = "uint64"
  show GoString = "string"
  show GoFloat = "float32"
  show GoDouble = "float64"
  show GoBool = "bool"
  show GoTime = "time.Time"

data GoTypes = GoBasic GoSimpleTypes | GoArrayLike (Maybe Int) GoTypes | GoMap GoTypes GoTypes | GoStruct GoStructDef | GoInterface | GoTyVar String | GoPointer GoTypes

instance Show GoTypes where
  show (GoBasic t) = show t
  show (GoArrayLike Nothing t) = "[]" ++ show t
  show (GoArrayLike (Just n) t) = '[' : showInt n ("]" ++ show t)
  show (GoMap t1 t2) = "map[" ++ show t1 ++ "]" ++ show t2
  show (GoStruct defs) = show defs
  show GoInterface = "interface{}"
  show (GoTyVar t) = '$' : t
  show (GoPointer t) = '*' : show t

goTypeLit = goPointer <|> goInterface <|> fmap GoBasic goBasicTypeLit <|> goArrayLike <|> goMap <|> fmap GoStruct goStruct <|> goTyVar
  where
    goPointer = char '*' *> (GoPointer <$> goTypeLit)
    goInterface = (string "interface{}" <|> string "any") $> GoInterface
    goArrayLike = GoArrayLike <$> (goSlice <|> goArray) <*> goTypeLit
      where
        goSlice = string "[]" >> return Nothing
        goArray = between (char '[') (char ']') (Just <$> L.decimal)
    goMap = between (string "map[") (char ']') $ GoMap <$> goTypeLit <*> goTypeLit
    goTyVar = GoTyVar <$> goIdent

goBasicTypeLit :: Parser GoSimpleTypes
goBasicTypeLit =
  choice
    [ string "int8" >> return GoInt8,
      string "int16" >> return GoInt16,
      string "int32" >> return GoInt32,
      string "int64" >> return GoInt64,
      string "int" >> return GoInt,
      string "uint8" >> return GoUInt8,
      string "uint16" >> return GoUInt16,
      string "uint32" >> return GoUInt32,
      string "uint64" >> return GoUInt64,
      string "uint" >> return GoUInt,
      string "string" >> return GoString,
      string "float32" >> return GoFloat,
      string "float64" >> return GoDouble,
      string "bool" >> return GoBool,
      string "time.Time" >> return GoTime
    ]

goIdent :: Parser String
goIdent = some alphaNumChar

goStructLine :: Parser GoStructLine
goStructLine = do
  ident <- goIdent
  try
    ( do
        typ <- hspace1 *> goTypeLit
        tag <- (hspace1 *> goStructTags) <|> pure []
        return $ GoStructLine ident typ tag
    )
    <|> do
      tag <- (hspace1 *> goStructTags) <|> pure []
      return $ GoEmbedStruct ident tag

goStructTags :: Parser [JSONTags]
goStructTags = between (char '`') (char '`') jsonTags

jsonTags :: Parser [JSONTags]
jsonTags = jsonTags' <|> (anySingle *> jsonTags)

jsonTags' :: Parser [JSONTags]
jsonTags' = do
  string "json:"
  between (char '"') (char '"') $ do
    p1 <- jsonKeyName <|> pure []
    p2 <- jsonOpt <|> pure []
    return $ p1 ++ p2

jsonKeyName :: Parser [JSONTags]
jsonKeyName =
  many (satisfy isValidTag) >>= \case
    "" -> pure []
    "-" -> (notFollowedBy (char ',') $> [KeyIgnore]) <|> pure [KeyRename "-"]
    n -> pure [KeyRename n]
  where
    isValidTag c = isAlphaNum c || elem @[] c "!#$%&()*+-./:;<=>?@[]^_{|}~ "

jsonOpt :: Parser [JSONTags]
jsonOpt = char ',' *> (opts `sepEndBy` char ',')
  where
    opts =
      choice
        [ string "omitempty" $> KeyOmitEmpty,
          string "string" $> KeyAsString,
          KeyUnknown <$> some alphaNumChar
        ]

genExampleValue :: HM.HashMap String GoStructDef -> GoTypes -> Either String A.Value
genExampleValue _ (GoBasic t) = pure $ genSimple t
genExampleValue _ (GoArrayLike n' (GoBasic t))
  | n > 0 && n < 10 = pure $ Array $ V.take n (genSimpleArrayLike t)
  | otherwise = pure $ Array $ genSimpleArrayLike t
  where
    n = fromMaybe 5 n'
genExampleValue env (GoArrayLike n' t)
  | n > 0 && n < 6 = do
      val <- genExampleValue env t
      pure $ Array $ V.replicate n val
  | otherwise = do
      val <- genExampleValue env t
      pure $ Array $ V.fromList [val]
  where
    n = fromMaybe 5 n'
genExampleValue env (GoStruct (GoStructDef defs)) = jsonize'' env defs
genExampleValue env (GoTyVar t) = case HM.lookup t env of
  Just (GoStructDef def) -> jsonize'' (HM.delete t env) def
  Nothing -> Left $ "undefined or invalid: " ++ t
genExampleValue _ GoInterface = pure $ Object A.empty
genExampleValue env (GoPointer p) = deref 1 p
  where
    deref :: Int -> GoTypes -> Either String Value
    deref !i t@(GoPointer ptr)
      | i < 5 = deref (i + 1) ptr
      | otherwise = Left $ "deep pointer occurrence: " ++ replicate i '*' ++ show t
    deref _ (GoTyVar t) = case HM.lookup t env of
      Just (GoStructDef def) -> jsonize'' (HM.delete t env) def
      Nothing -> pure Null
    deref _ base = genExampleValue env base
genExampleValue env (GoMap t1 t2) = do
  key <- check t1
  val <- genExampleValue env t2
  pure $ Object $ A.fromList [(key, val)]
  where
    check (GoBasic GoString) = Right "myKey"
    check (GoBasic GoInt8) = Right "8"
    check (GoBasic GoInt16) = Right "16"
    check (GoBasic GoInt32) = Right "-32"
    check (GoBasic GoInt64) = Right "64"
    check (GoBasic GoInt) = Right "-1024"
    check (GoBasic GoUInt8) = Right "8"
    check (GoBasic GoUInt16) = Right "1616"
    check (GoBasic GoUInt32) = Right "323232"
    check (GoBasic GoUInt64) = Right "646464646464"
    check (GoBasic GoUInt) = Right "1024"
    check (GoBasic GoFloat) = Right "1.26"
    check (GoBasic GoDouble) = Right "16.3"
    check (GoBasic GoTime) = Right "2009-11-10T23:00:00Z"
    check (GoBasic GoBool) = Left $ "json: unsupported type: map[bool]" ++ show t2
    check t@(GoArrayLike _ _) = Left $ "json: unsupported type: map[" ++ show t ++ "]" ++ show t2
    check t@(GoMap _ _) = Left $ "json: unsupported type: map[" ++ show t ++ "]" ++ show t2
    check t@(GoStruct _) = Left $ "json: unsupported type: map[" ++ show t ++ "]" ++ show t2
    check GoInterface = Left $ "json: unsupported type: map[interface{}]" ++ show t2
    check (GoTyVar t) = Left $ "json: unsupported type: map[main." ++ t ++ "]" ++ show t2
    check (GoPointer t) = Left $ "json: unsupported type: map[*" ++ show t ++ "]" ++ show t2

{--
  While it's fine to have types like map[bool]int, map[bool]bool
  Go will fail with "json: unsupported type: map[bool]bool" when marshalling,
  also: map[interface{}]interface{}
--}

genSimpleArrayLike :: GoSimpleTypes -> V.Vector A.Value
genSimpleArrayLike = V.fromList . go
  where
    go GoBool = map Bool [False, True, True, False, True, False, False, False, True, False]
    go GoInt = map Number [1, -2, 3, -4, 5, -6, 7, -8, 9, 0]
    go GoInt8 = map Number [-8, 16, 32, -64, -128, 0, 1, 2, 4, 8]
    go GoInt16 = map Number [16, 17, 18, 19, -20, 21, 22, 23, -24, -25]
    go GoInt32 = map Number [32, 3, -3, 4, 6, -4, 0, 2, -5, 8]
    go GoInt64 = map Number [9223372036854775800, 922, 10, -9223372, 9223372036854, -1234, 56, -7, 10240, 65536]
    go GoUInt = map Number [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]
    go GoUInt8 = map Number [8, 16, 32, 64, 128, 0, 1, 2, 4, 8]
    go GoUInt16 = map Number [16, 17, 18, 19, 20, 21, 22, 23, 24, 25]
    go GoUInt32 = map Number [32, 3, 3, 4, 6, 4, 0, 2, 5, 8]
    go GoUInt64 = map Number [9223372036854775800, 922, 10, 9223372, 9223372036854, 1234, 56, 7, 10240, 65536]
    go GoFloat = map Number [9.5, 3.8, 1.13, 4.79, 5.023, 6.1, 2.7234, 1.9, 1.02, 5.64]
    go GoDouble = map Number [8.5397342, 4.26986711133, 871.0528907127, 9.869604, 31.00627668, 10.24, 16.3, 12.6, 12.0001, 564.222]
    go GoString = map String ["rob pike", "Robert", "Pike", "gopher", "Gopher", "Go", "Golang", "goroutine", "interface{}", "struct"]
    go GoTime = map String ["2011-01-26T19:06:43Z", "2020-07-16T14:49:50.3269159+08:00", "2011-01-26T19:01:12Z", "2011-01-26T19:14:43Z", "2009-11-10T23:00:00Z", "2018-09-22T12:42:31Z", "2020-12-29T14:58:15Z", "2006-01-02T15:04:05Z", "2020-12-29T14:58:15.229579703+08:00", "2017-12-30T11:25:30+09:00"]
{-# INLINE genSimpleArrayLike #-}

genSimple :: GoSimpleTypes -> Value
genSimple GoInt = Number 1
genSimple GoInt8 = Number 127
genSimple GoInt16 = Number (-1616)
genSimple GoInt32 = Number 60000
genSimple GoInt64 = Number 64
genSimple GoUInt = Number 1
genSimple GoUInt8 = Number 255
genSimple GoUInt16 = Number 1616
genSimple GoUInt32 = Number 323232
genSimple GoUInt64 = Number 646464646464
genSimple GoString = String "robpike"
genSimple GoFloat = Number 2.71828
genSimple GoDouble = Number 3.1415926
genSimple GoBool = Bool True
genSimple GoTime = String "2006-01-02T15:04:05Z"
{-# INLINE genSimple #-}

genSimple' :: GoSimpleTypes -> String
genSimple' GoInt = "0"
genSimple' GoInt8 = "1"
genSimple' GoInt16 = "2"
genSimple' GoInt32 = "4"
genSimple' GoInt64 = "8"
genSimple' GoUInt = "0"
genSimple' GoUInt8 = "255"
genSimple' GoUInt16 = "65535"
genSimple' GoUInt32 = "4294967295"
genSimple' GoUInt64 = "18446744073709551615"
genSimple' GoString = "\"___\""
genSimple' GoFloat = "2.71828"
genSimple' GoDouble = "3.1415926"
genSimple' GoBool = "false"
genSimple' GoTime = "2006-01-02T15:04:05Z"
{-# INLINE genSimple' #-}

jsonize defs = fmap prettyJson (prepareEnv >>= jsonize' defs)
  where
    prettyJson = toStrict . mconcat . intersperse "\n" . map encodePretty
    prepareEnv = foldM g HM.empty [(name, val) | (Just name, val) <- defs]
      where
        g m (k, v)
          | k `HM.member` m = Left $! k ++ " redeclared in this block"
          | otherwise = Right (HM.insert k v m)

jsonize' :: [(Maybe String, GoStructDef)] -> HM.HashMap String GoStructDef -> Either String [Value]
jsonize' [] _ = return []
jsonize' ((name, GoStructDef xs) : defs) env = liftA2 (:) (jsonize'' env' xs) (jsonize' defs env)
  where
    env' = maybe env (`HM.delete` env) name

jsonize'' :: HM.HashMap String GoStructDef -> [GoStructLine] -> Either String Value
jsonize'' env = fmap object . pairize env

pairize environ = foldlM (go environ) []
  where
    go env ps (GoEmbedStruct tv tags)
      | KeyIgnore `elem` tags = return ps
      | otherwise =
          case HM.lookup tv env of
            Just (GoStructDef defs) -> do
              ps' <- pairize (HM.delete tv env) defs
              if null ident'
                then return $! ps' ++ ps
                else return $! (A.fromString (head ident') .= object ps') : ps
            Nothing -> Left $! "undefined or invalid: " ++ tv
      where
        ident' = [n | KeyRename n <- tags]
    go env ps (GoStructLine ident t tags)
      | KeyIgnore `elem` tags = example >> return ps
      | notExported && not (null tags) = Left $! "struct field " ++ ident ++ " has json tag but is not exported"
      | notExported = example >> return ps
      | otherwise = do
          eg <- example
          return $! (A.fromString ident' .= eg) : ps
      where
        notExported = not $ isUpper $ head ident
        ident' = head $ [n | KeyRename n <- tags] <|> [ident]
        example = case t of
          GoBasic b | KeyAsString `elem` tags -> pure $ String $ T.pack $ genSimple' b
          -- json tag "string" expecting: string, floating point, integer, or boolean types
          -- other types ignore it, just proceed as usual
          _ -> genExampleValue env t
