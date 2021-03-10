{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lib (getJson) where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (foldM, guard, liftM2, replicateM, (<=<))
import Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Attoparsec.Text as P
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char (isAlphaNum, isPrint, isSpace, isUpper)
import Data.Foldable (foldlM)
import qualified Data.HashMap.Strict as HM
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Vector as V
import Numeric (showInt)

getJson = either pack jsonize . (parseOnly goStructBlock <=< first show . decodeUtf8')
  where
    goStructBlock = skipMany endOfLine' *> goStructs <* ((comment `sepBy` skipSpace) >> skipSpace >> endOfInput)
    comment = skipSpace >> string "//" >> skipWhile (not . isEndOfLine)

newtype GoStructDef = GoStructDef [GoStructLine]

instance Show GoStructDef where
  show (GoStructDef []) = "struct {}"
  show (GoStructDef defs) = "struct {\n" ++ concatMap (\line -> "    " ++ show line) defs ++ "}"

data GoStructLine = GoStructLine Text GoTypes [JSONTags] | GoEmbedStruct Text [JSONTags] -- Id type tags

instance Show GoStructLine where
  show (GoStructLine k typ _) = let k' = T.unpack k in k' ++ " " ++ show typ ++ " `json:\"" ++ k' ++ "\"`\n"
  show (GoEmbedStruct t _) = T.unpack t

data JSONTags = KeyRename Text | KeyOmitEmpty | KeyIgnore | KeyAsString deriving (Show, Eq)

goStructs = (typedef <|> fmap (Nothing,) goStruct) `sepBy` skipMany1 endOfLine'

typedef = liftA2 (,) name def
  where
    name = (string "type" >> skipSpace') *> fmap Just goIdent
    def = skipSpace' *> goStruct

goStruct = string "struct" >> goBlocky (withComments definitions)
  where
    definitions = GoStructDef <$> (goStructLine `sepBy` skipMany1 endOfLine')
    withComments p = skipMany endOfLine' *> p <* skipMany endOfLine'

endOfLine' = do
  skipWhile isHorizontalSpace
  trailingComment <|> endOfLine
  where
    trailingComment = do
      string "//"
      skipWhile (not . isEndOfLine)
      endOfLine

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

data GoTypes = GoBasic GoSimpleTypes | GoArrayLike Int GoTypes | GoMap GoTypes GoTypes | GoStruct GoStructDef | GoInterface | GoTyVar Text | GoPointer GoTypes

instance Show GoTypes where
  show (GoBasic t) = show t
  show (GoArrayLike n t) = let t' = "]" ++ show t in "[" ++ if n > 0 then showInt n t' else t'
  show (GoMap t1 t2) = "map[" ++ show t1 ++ "]" ++ show t2
  show (GoStruct defs) = show defs
  show GoInterface = "interface{}"
  show (GoTyVar t) = show t
  show (GoPointer t) = '*' : show t

goBlocky p = begin *> p <* end
  where
    begin = skipSpace >> char '{'
    end = skipSpace >> char '}'

goTypeLit = goPointer <|> goInterface <|> fmap GoBasic goBasicTypeLit <|> goArrayLike <|> goMap <|> fmap GoStruct goStruct <|> goTyVar
  where
    goPointer = char '*' >> (GoPointer <$> goTypeLit)
    goInterface = string "interface{}" >> pure GoInterface
    goArrayLike = do
      n <- goSlice <|> goArray
      GoArrayLike n <$> goTypeLit
      where
        goSlice = string "[]" >> return 0
        goArray = do
          char '['
          n <- decimal
          guard (n > 0)
          char ']'
          return n
    goMap = do
      string "map["
      keyType <- goTypeLit
      char ']'
      GoMap keyType <$> goTypeLit
    goTyVar = GoTyVar <$> goPublicIdent

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

skipSpace' = skipMany1 (satisfy isHorizontalSpace)

goIdent = P.takeWhile1 isAlphaNum

goPublicIdent = do
  i <- goIdent
  i <$ guard (isUpper $ T.head i)

goStructLine = goPlainStructLine <|> goEmbeddedStructLine
  where
    goPlainStructLine = GoStructLine <$> (skipSpace *> goIdent) <*> (skipSpace' *> goTypeLit) <*> ((skipSpace' *> goStructTags) <|> pure [])
    goEmbeddedStructLine = GoEmbedStruct <$> (skipSpace *> goPublicIdent) <*> ((skipSpace' *> goStructTags) <|> pure [])

goStructTags = do
  (a, b) <- char '`' *> (T.breakOn "json:\"" <$> P.takeWhile (\c -> c /= '`' && isPrint c)) <* char '`'
  if T.null a && T.null b
    then return []
    else case T.break (== '"') $ T.drop 6 b of
      (c, d) | (T.null a || isSpace (T.last a)) && not (T.null d) && T.head d == '"' -> return (tags c)
      _ -> return []
  where
    tags "" = []
    tags "-" = [KeyIgnore]
    tags ts = if T.null x then p1 xs else KeyRename x : p1 xs
      where
        (x : xs) = T.split (== ',') ts
        p1 = mapMaybe $ \case
          "omitempty" -> Just KeyOmitEmpty
          "string" -> Just KeyAsString
          _ -> Nothing

genExampleValue :: HM.HashMap Text GoStructDef -> GoTypes -> Either String A.Value
genExampleValue _ (GoBasic t) = return $! genSimple t
genExampleValue _ (GoArrayLike n (GoBasic t))
  | n > 0 && n < 10 = return $! Array $ V.take n (genSimpleArrayLike t)
  | otherwise = return $! Array $ genSimpleArrayLike t
genExampleValue env (GoArrayLike n t)
  | n > 0 && n < 5 = do
    val <- replicateM n (genExampleValue env t)
    return $! Array $ V.fromList val
  | otherwise = do
    val <- genExampleValue env t
    return $! Array $ V.fromList [val]
genExampleValue env (GoStruct (GoStructDef defs)) = jsonize'' env defs
genExampleValue env (GoTyVar t) = case HM.lookup t env of
  Just (GoStructDef def) -> jsonize'' (HM.delete t env) def
  Nothing -> Left $! "undefined or invalid: " ++ T.unpack t
genExampleValue _ GoInterface = return $! Object HM.empty
genExampleValue env (GoPointer p) = deref 1 p
  where
    deref :: Int -> GoTypes -> Either String Value
    deref !i t@(GoPointer ptr)
      | i < 5 = deref (i + 1) ptr
      | otherwise = Left $! "deep pointer occurrence: " ++ replicate i '*' ++ show t
    deref _ (GoTyVar t) = case HM.lookup t env of
      Just (GoStructDef def) -> jsonize'' (HM.delete t env) def
      Nothing -> return Null
    deref _ base = genExampleValue env base
genExampleValue env (GoMap t1 t2) = do
  key <- check t1
  val <- genExampleValue env t2
  return $! Object $ HM.fromList [(key, val)]
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
    check (GoBasic GoBool) = Left $! "json: unsupported type: map[bool]" ++ show t2
    check t@(GoArrayLike _ _) = Left $! "json: unsupported type: map[" ++ show t ++ "]" ++ show t2
    check t@(GoMap _ _) = Left $! "json: unsupported type: map[" ++ show t ++ "]" ++ show t2
    check t@(GoStruct _) = Left $! "json: unsupported type: map[" ++ show t ++ "]" ++ show t2
    check GoInterface = Left $! "json: unsupported type: map[interface{}]" ++ show t2
    check (GoTyVar t) = Left $! "json: unsupported type: map[main." ++ T.unpack t ++ "]" ++ show t2
    check (GoPointer t) = Left $! "json: unsupported type: map[*" ++ show t ++ "]" ++ show t2

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

genSimple' :: GoSimpleTypes -> Text
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

jsonize defs = either pack prettyJson (prepareEnv >>= jsonize' defs)
  where
    prettyJson = mconcat . intersperse "\n" . map encodePretty
    prepareEnv = foldM g HM.empty [(name, val) | (Just name, val) <- defs]
      where
        g m (k, v)
          | k `HM.member` m = Left $! T.unpack k ++ " redeclared in this block"
          | otherwise = Right (HM.insert k v m)

jsonize' :: [(Maybe Text, GoStructDef)] -> HM.HashMap Text GoStructDef -> Either String [Value]
jsonize' [] _ = return []
jsonize' ((name, GoStructDef xs) : defs) env = liftM2 (:) (jsonize'' env' xs) (jsonize' defs env)
  where
    env' = maybe env (`HM.delete` env) name

jsonize'' :: HM.HashMap Text GoStructDef -> [GoStructLine] -> Either String Value
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
              else return $! (head ident' .= object ps') : ps
          Nothing -> Left $! "undefined or invalid: " ++ T.unpack tv
      where
        ident' = [n | KeyRename n <- tags]
    go env ps (GoStructLine ident t tags)
      | KeyIgnore `elem` tags = return ps
      | otherwise = do
        eg <- example
        return $! (ident' .= eg) : ps
      where
        ident' = head $ [n | KeyRename n <- tags] <|> [ident]
        example = case t of
          GoBasic b | KeyAsString `elem` tags -> return $! String (genSimple' b)
          -- json tag "string" expecting: string, floating point, integer, or boolean types
          -- other types ignore it, just proceed as usual
          _ -> genExampleValue env t
