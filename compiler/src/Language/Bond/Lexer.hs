-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.Bond.Lexer
    ( angles
    , braces
    , brackets
    , colon
    , comma
    , commaEnd
    , commaEnd1
    , commaSep1
    , decimal
    , equal
    , float
    , identifier
    , namespaceIdentifier
    , integer
    , keyword
    , lexeme
    , natural
    , parens
    , unescapedStringLiteral
    , semi
    , semiEnd
    , semiOrCommaEnd
    , semiOrCommaSep
    , semiOrCommaSep1
    , semiOrCommaSepEnd
    , semiOrCommaSepEnd1
    , semiSep
    , stringLiteral
    , symbol
    , whiteSpace
    ) where

import Data.List
import Data.Void (Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

rws :: [String] -- list of reserved words
rws = [ "blob"
      , "bond_meta"
      , "bonded"
      , "bool"
      , "class"
      , "double"
      , "enum"
      , "false"
      , "float"
      , "import"
      , "int16"
      , "int32"
      , "int64"
      , "int8"
      , "list"
      , "map"
      , "namespace"
      , "nullable"
      , "optional"
      , "required"
      , "required_optional"
      , "Schema"
      , "sealed"
      , "service"
      , "set"
      , "string"
      , "struct"
      , "true"
      , "uint16"
      , "uint32"
      , "uint64"
      , "uint8"
      , "using"
      , "var"
      , "vector"
      , "view_of"
      , "void"
      , "wstring"
      ]

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

colon :: Parser String
colon = symbol ":"

comma :: Parser String
comma = symbol ","

commaSep1 p = sepBy1 p comma

decimal = lexeme L.decimal

identifier' :: [String] -> Parser String
identifier' restricted = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` restricted
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

identifier = identifier' rws

integer' :: Parser Integer
integer' = lexeme L.decimal

integer = L.signed sc integer'

keyword :: String -> Parser ()
keyword w = lexeme (string w *> notFollowedBy alphaNumChar)

hexadecimal :: Parser Integer
hexadecimal = lexeme $ char '0' >> char' 'x' >> L.hexadecimal

octal :: Parser Integer
octal = lexeme $ char '0' >> char' 'o' >> L.octal

natural = decimal <|> hexadecimal <|> octal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

semi :: Parser String
semi = symbol ";"

semiSep p = sepBy p semi

symbol :: String -> Parser String
symbol = L.symbol sc

whiteSpace = sc

namespaceIdentifier = identifier' (delete "Schema" rws)

equal = symbol "="
semiEnd p = endBy p semi
commaEnd p = endBy p comma
commaEnd1 p = endBy1 p comma

semiOrComma = semi <|> comma

semiOrCommaSep p = sepBy p semiOrComma
semiOrCommaSep1 p = sepBy1 p semiOrComma
semiOrCommaEnd p = endBy p semiOrComma
semiOrCommaSepEnd p = sepEndBy p semiOrComma
semiOrCommaSepEnd1 p = sepEndBy1 p semiOrComma

quote = symbol "\""
quotes = between quote quote

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

unescapedStringLiteral = quotes $ many $ satisfy (/= '"')

float' :: Parser Double
float' = lexeme L.float

float :: Parser Double
float = L.signed sc float'

