module Parser
( --module Text.Parsec
Parser
, anyChar
, chainl
, char
, digits
, eof
, eol
, letters
, lineDigit
, line2Digits
, lookAhead
, many
, manyTill
, nonDigits
, nonDigits1
, nonSignedDigits
, nonSignedDigits1
, parse
, sepBy
, sepEndBy
, signedFloat
, signedInt
, string
, try
, whitespace
, whitespace1
, whitespaceEOL
, unsignedInt
, (<|>)
) where

import Text.Parsec.Text (Parser)
import Text.Parsec


eol :: Parser Char
eol = char '\n'

whitespaceEOL :: Parser String
whitespaceEOL = many $ oneOf " \t\n"

whitespace :: Parser String
whitespace = many $ oneOf " \t"

whitespace1 :: Parser String
whitespace1 = many1 $ oneOf " \t"

letters :: Parser String
letters = many1 letter

digits :: Parser Int
digits = read <$> many1 digit

nonDigits :: Parser String
nonDigits = many $ noneOf "0123456789"

nonDigits1 :: Parser String
nonDigits1 = many1 $ noneOf "0123456789"

nonSignedDigits :: Parser String
nonSignedDigits = many $ noneOf "0123456789-+"

nonSignedDigits1 :: Parser String
nonSignedDigits1 = many1 $ noneOf "0123456789-+"

unsignedInt :: Parser Int
unsignedInt = read <$> many1 digit

signedInt :: Parser Int
signedInt = do plus <|> minus <|> unsignedInt
    where plus = char '+' >> unsignedInt
          minus = negate <$> (char '-' >> unsignedInt)

signedFloat :: Parser Float
signedFloat = do s <- option 1.0 ((char '+' >> return 1.0) <|>
                                  (char '-' >> return (-1.0)))
                 i <- many1 digit
                 d <- option ".0" ((:) <$> char '.' <*> many1 digit)
                 return . (*s) . read $ i ++ d

lineDigit :: Parser Int
lineDigit = (nonDigits >> digits) <* nonDigits <* eol

line2Digits :: Parser (Int, Int)
line2Digits = (,) <$> (nonDigits >> digits) <*> (nonDigits >> digits) <* eol
