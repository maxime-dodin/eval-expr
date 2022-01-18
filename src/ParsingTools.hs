module ParsingTools where

{-# LANGUAGE TupleSections #-}
import Data.Either
import Text.Read (readEither)
import Control.Applicative

data Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser fM where
        fM str = case runParser parser str of
            Left x -> Left x
            Right (v,w) -> Right (fct v, w)

instance Applicative Parser where
    pure a = Parser f where
        f str = Right (a, str)
    (<*>) f x = Parser fStar where
        fStar str = case runParser f str of
            Left msg -> Left msg
            Right (v,v') -> case runParser x v' of
                Left msg -> Left msg
                Right (w,w') -> Right (v w, w')

instance Alternative Parser where
    (<|>) a b = Parser func where
        func str = case runParser a str of
            Left x -> runParser b str
            Right x -> Right x
    empty = Parser (const (Left "empty"))

instance Monad Parser where
    return = pure
    (>>=) a f = Parser p where
        p str = case runParser (f <$> a) str of
            Right (b, str) -> runParser b str
            Left str -> Left str

parseChar :: Char -> Parser Char
parseChar char = Parser func where
    func [] = Left "error: empty string to scan."
    func (x:xs)
        | char == x = Right (x,xs)
        | otherwise = Left ("can't find \'" ++ [char] ++ "\' in \'" ++ (x:xs) ++ "\'")

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser func where
    func [] = Left "error: empty string to scan"
    func _ = Left "error: char not found"
parseAnyChar (x:xs) = Parser func where
    func str = case runParser (parseChar x) str of
        Left _ -> runParser (parseAnyChar xs) str
        Right (x,xs) -> Right (x,xs)

parseOr :: Parser a -> Parser a -> Parser a
parseOr a b = Parser func where
    func str = case runParser a str of
        Left x -> runParser b str
        Right x -> Right x

parseMany ::  Parser a -> Parser [a]
parseMany func = Parser pMany where
    pMany str = case runParser func str of
        Right (x, y) -> case runParser (parseMany func) y of
            Right (v,w) -> Right (x:v, w)
            Left x -> Right ([], str)
        Left x -> Right ([], str)

parseSome :: Parser a -> Parser [a]
parseSome func = Parser pSome where
    pSome str = case runParser func str of
        Right (x, y) -> case runParser (parseMany func) y of
            Right (v,w) -> Right (x:v, w)
            Left x -> Right ([], str)
        Left x -> Left x

parseCheck :: (Char -> Bool) -> Parser Char
parseCheck f = Parser $ \s -> case s of
  (x:xs) | f x -> Right (x, xs)
  _            -> Left "error: blank"

parseSpace :: Parser a -> Parser a
parseSpace p = blanks *> p <* blanks
    where
        blanks = parseMany (parseCheck (== '\t') <|> parseCheck (== ' '))

parseSign :: Parser Double
parseSign = parseOr ((-1) <$ parseChar '-') (pure 1)

readDouble :: String -> Either String Double
readDouble = readEither

parseUDouble :: Parser Double
parseUDouble = Parser f where
    f str = case runParser (readDouble <$> parseSome (parseAnyChar "0123456789.")) str of
        Left msg -> Left msg
        Right (Right nb, s) -> Right (nb,s)
        Right (Left x, s) -> Left "error: udouble"

parseDouble :: Parser Double
parseDouble = (*) <$> parseSign <*> parseSpace parseUDouble