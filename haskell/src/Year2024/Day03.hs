module Year2024.Day03 where
import AdventOfCode

data Stmt = N Int | Do | Dont deriving (Eq, Show)

statements :: Parser [Stmt]
statements = noise >> many (stmt <* noise)
  where stmt = (try mul <|> try do' <|> try dont)
        mul = do _ <- string "mul("
                 i <- (*) <$> digits <* char ',' <*> (digits <* string ")")
                 return $ N i
        do' = string "do()" >> return Do
        dont = string "don't()" >> return Dont
        noise = manyTill (anyChar <|> eol) nextOrEOF
        nextOrEOF = (try $ lookAhead stmt >> return ()) <|> eof

eval :: [Stmt] -> Int
eval [] = 0
eval ((N i):xs) = i + eval xs
eval (_:xs) = eval xs

eval' :: Bool -> [Stmt] -> Int
eval' _ [] = 0
eval' True ((N i):xs) = i + eval' True xs
eval' False ((N _):xs) =  eval' False xs
eval' _ (Do:xs) = eval' True xs
eval' _ (Dont:xs) = eval' False xs

solve :: IO (Int, Int)
solve = do input <- pack <$> readFile "data/Year2024/day03.txt"
           let ps = fromRight [] $ parse statements "" input
           return (eval ps, eval' True ps)
