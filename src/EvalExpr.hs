module EvalExpr where

import ParsingTools
import Control.Applicative
import System.IO (putStr)

data Expr = Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Num Double
    deriving (Show, Eq)

parseAdd :: Parser Expr
parseAdd = (Add <$> parseSpace parseSub <*> (parseChar '+' *> parseAdd)) <|> parseSub

parseSub :: Parser Expr
parseSub = (Sub <$> parseSpace parseMul <*> (parseChar '-' *> parseSub)) <|> parseMul

parseMul :: Parser Expr
parseMul = (Mul <$> parseSpace parseDiv <*> (parseChar '*' *> parseMul)) <|> parseDiv

parseDiv :: Parser Expr
parseDiv = (Div <$> parseSpace parsePow <*> (parseChar '/' *> parseDiv)) <|> parsePow

parsePow :: Parser Expr
parsePow = (Pow <$> parseSpace parseNum <*> (parseChar '^' *> parsePow)) <|> parseNum

parseNum :: Parser Expr
parseNum = (Num <$> parseSpace parseDouble) <|> parseSpace (parseChar '(' *> parseAdd <* parseChar ')')

eval :: Expr -> Either String Double
eval (Add x x') = (+) <$> eval x <*> eval x'
eval (Sub (Num a) (Sub (Num b) c)) = eval (Sub (Num (a - b)) c)
eval (Sub x x') = (-) <$> eval x <*> eval x'
eval (Mul x x') = (*) <$> eval x <*> eval x'
eval (Div x (Num 0)) = Left "division by zero"
eval (Div (Num a) (Div (Num b) c)) = eval (Div (Num (a / b)) c)
eval (Div x x') = (/) <$> eval x <*> eval x'
eval (Pow x x') = (**) <$> eval x <*> eval x'
eval (Num x) = Right x

evalExpr :: String -> Either String Double
evalExpr s = case runParser parseAdd s of
    Left str -> Left str
    Right(x, "") -> case eval x of
        Right val -> Right val
        Left msg -> Left msg
    Right (x, x') -> Left $ "left to parse: " ++ x'