{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lib (getJson) where

import Control.Applicative
import Control.Monad (guard)
import Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Attoparsec.Text as P
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', intersperse)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V
import Numeric (showInt)

getJson = either pack jsonize . parseOnly goStructBlock . decodeUtf8
  where
    goStructBlock = skipMany endOfLine' *> goStructs <* ((comment `sepBy` skipSpace) >> skipSpace >> endOfInput)
    comment = skipSpace >> string "//" >> skipWhile (not . isEndOfLine)

newtype GoStructDef = GoStructDef [GoStructLine]

instance Show GoStructDef where
  show (GoStructDef []) = "struct {}"
  show (GoStructDef defs) = "struct {\n" ++ concatMap (\line -> "    " ++ show line) defs ++ "}"

data GoStructLine = GoStructLine Text GoTypes [JSONTags] -- Id type tags

instance Show GoStructLine where
  show (GoStructLine k typ _) = let k' = T.unpack k in k' ++ " " ++ show typ ++ " `json:\"" ++ k' ++ "\"`\n"

data JSONTags = KeyRename Text | KeyOmitEmpty | KeyIgnore | KeyAsString deriving (Show, Eq)

goStructs = (typedef <|> fmap ("",) goStruct) `sepBy` skipMany1 endOfLine'

typedef = liftA2 (,) name def
  where
    name = (string "type" >> skipSpace') *> goIdent
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

data GoSimpleTypes = GoInt8 | GoInt16 | GoInt32 | GoInt64 | GoInt | GoString | GoDouble | GoFloat | GoBool deriving (Eq)

instance Show GoSimpleTypes where
  show GoInt = "int"
  show GoInt8 = "int8"
  show GoInt16 = "int16"
  show GoInt32 = "int32"
  show GoInt64 = "int64"
  show GoString = "string"
  show GoFloat = "float32"
  show GoDouble = "float64"
  show GoBool = "bool"

data GoTypes = GoBasic GoSimpleTypes | GoArrayLike Int GoTypes | GoMap GoSimpleTypes GoTypes | GoStruct GoStructDef | GoInterface | GoNil | GoTyVar Text

instance Show GoTypes where
  show (GoBasic t) = show t
  show (GoArrayLike n t) = let t' = "]" ++ show t in "[" ++ if n > 0 then showInt n t' else t'
  show (GoMap t1 t2) = "map[" ++ show t1 ++ "]" ++ show t2
  show (GoStruct defs) = show defs
  show GoInterface = "interface{}"
  show (GoTyVar t) = show t
  show GoNil = "nil"

goBlocky p = begin *> p <* end
  where
    begin = skipSpace >> char '{'
    end = skipSpace >> char '}'

goTypeLit = goInterface <|> fmap GoBasic goBasicTypeLit <|> goArrayLike <|> goMap <|> fmap GoStruct goStruct <|> goTyVar
  where
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
      keyType <- goBasicTypeLit
      char ']'
      GoMap keyType <$> goTypeLit
    goTyVar = do
      tv <- (char '*' *> goIdent) <|> goIdent -- takeWhile1
      GoTyVar tv <$ guard (isUpper $ T.head tv)

goBasicTypeLit =
  choice
    [ string "int8" >> return GoInt8,
      string "int16" >> return GoInt16,
      string "int32" >> return GoInt32,
      string "int64" >> return GoInt64,
      string "int" >> return GoInt,
      string "string" >> return GoString,
      string "float32" >> return GoFloat,
      string "float64" >> return GoDouble,
      string "bool" >> return GoBool
    ]

skipSpace' = skipMany1 (satisfy isHorizontalSpace)

goIdent = P.takeWhile1 isAlphaNum

goStructLine = GoStructLine <$> (skipSpace *> goIdent) <*> (skipSpace' *> goTypeLit) <*> ((skipSpace' *> goStructTags) <|> pure [])

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

genExampleValue :: HM.HashMap Text GoStructDef -> GoTypes -> A.Value
genExampleValue _ (GoBasic t) = genSimple t
genExampleValue _ (GoArrayLike n (GoBasic t))
  | n > 0 && n < 10 = Array $ V.take n (genSimpleArrayLike t)
  | otherwise = Array $ genSimpleArrayLike t
genExampleValue env (GoArrayLike n t)
  | n > 0 && n < 5 = Array $ V.fromList $ replicate n (genExampleValue env t)
  | otherwise = Array $ V.fromList [genExampleValue env t]
genExampleValue _ (GoMap t1 t2) | t1 == GoBool = String $ "json: unsupported type: map[bool]" <> T.pack (show t2)
genExampleValue env (GoMap t1 t2) = Object $ HM.fromList [(showKey t1, genExampleValue env t2)]
genExampleValue env (GoStruct (GoStructDef defs)) = jsonize'' env defs
genExampleValue env (GoTyVar t) = maybe Null (\(GoStructDef def) -> jsonize'' (HM.delete t env) def) (HM.lookup t env)
genExampleValue _ GoInterface = Object HM.empty
genExampleValue _ GoNil = Null

{--
  While it's fine to have types like map[bool]int, map[bool]bool
  Go will fail with "json: unsupported type: map[bool]bool" when marshalling,
  also: map[interface{}]interface{}
--}

genSimpleArrayLike :: GoSimpleTypes -> V.Vector A.Value
genSimpleArrayLike GoBool = V.fromList [Bool False, Bool True, Bool True, Bool False, Bool True, Bool False, Bool False, Bool False, Bool True, Bool False]
genSimpleArrayLike GoInt = V.fromList [Number 1, Number 2, Number 3, Number 4, Number 5, Number 6, Number 7, Number 8, Number 9, Number 0]
genSimpleArrayLike GoInt8 = V.fromList [Number 8, Number 16, Number 32, Number 64, Number 128, Number 0, Number 1, Number 2, Number 4, Number 8]
genSimpleArrayLike GoInt16 = V.fromList [Number 16, Number 17, Number 18, Number 19, Number 20, Number 21, Number 22, Number 23, Number 24, Number 25]
genSimpleArrayLike GoInt32 = V.fromList [Number 32, Number 3, Number 3, Number 4, Number 6, Number 4, Number 0, Number 2, Number 5, Number 8]
genSimpleArrayLike GoInt64 = V.fromList [Number 9223372036854775800, Number 922, Number 10, Number 9223372, Number 9223372036854, Number 1234, Number 56, Number 7, Number 10240, Number 65536]
genSimpleArrayLike GoFloat = V.fromList [Number 9.5, Number 3.8, Number 1.13, Number 4.79, Number 5.023, Number 6.1, Number 2.7234, Number 1.9, Number 1.02, Number 5.64]
genSimpleArrayLike GoDouble = V.fromList [Number 8.5397342, Number 4.26986711133, Number 871.0528907127, Number 9.869604, Number 31.00627668, Number 10.24, Number 16.3, Number 12.6, Number 12.0001, Number 564.222]
genSimpleArrayLike GoString = V.fromList [String "rob pike", String "Robert", String "Pike", String "gopher", String "Gopher", String "Go", String "Golang", String "goroutine", String "interface{}", "struct"]
{-# INLINE genSimpleArrayLike #-}

showKey :: GoSimpleTypes -> Text
showKey GoString = "myKey"
showKey GoInt8 = "8"
showKey GoInt16 = "16"
showKey GoInt32 = "32"
showKey GoInt64 = "64"
showKey GoInt = "1024"
showKey GoFloat = "1.26"
showKey GoDouble = "16.3"
showKey GoBool = "bool as key: not supported by encoding/json"
{-# INLINE showKey #-}

genSimple :: GoSimpleTypes -> Value
genSimple GoInt = Number 1
genSimple GoInt8 = Number 127
genSimple GoInt16 = Number 1616
genSimple GoInt32 = Number 60000
genSimple GoInt64 = Number 64
genSimple GoString = String "robpike"
genSimple GoFloat = Number 2.71828
genSimple GoDouble = Number 3.1415926
genSimple GoBool = Bool True
{-# INLINE genSimple #-}

genSimple' :: GoSimpleTypes -> Text
genSimple' GoInt = "0"
genSimple' GoInt8 = "1"
genSimple' GoInt16 = "2"
genSimple' GoInt32 = "4"
genSimple' GoInt64 = "8"
genSimple' GoString = "\"___\""
genSimple' GoFloat = "2.71828"
genSimple' GoDouble = "3.1415926"
genSimple' GoBool = "false"
{-# INLINE genSimple' #-}

jsonize defs = mconcat $ intersperse "\n" $ map encodePretty $ jsonize' env defs
  where
    env = HM.fromList [d | d@(name, _) <- defs, not (T.null name)]

jsonize' _ [] = []
jsonize' env (("", GoStructDef xs) : defs) = jsonize'' env xs : jsonize' env defs
jsonize' env ((name, GoStructDef xs) : defs) = jsonize'' (HM.delete name env) xs : jsonize' env defs

jsonize'' env xs = object (foldl' go [] xs)
  where
    go ys (GoStructLine ident t tags)
      | KeyIgnore `elem` tags = ys
      | otherwise = (ident' .= example) : ys
      where
        ident' = fromMaybe ident $ listToMaybe $ [n | KeyRename n <- tags]
        example
          | KeyAsString `elem` tags =
            case t of
              GoBasic b -> String (genSimple' b)
              GoTyVar tv | isNothing (HM.lookup tv env) -> Null
              _ -> String "expecting: string, floating point, integer, or boolean types"
          | otherwise = genExampleValue env t
