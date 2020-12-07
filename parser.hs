{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Char
import Control.Applicative
import Numeric
import System.IO
import Data.Map (Map, insert, fromList)

data Input = Input
    { inputLoc :: Int
    , inputStr :: String
    } deriving (Show, Eq)


inputUncons :: Input -> Maybe (Char, Input)
inputUncons (Input _ []) = Nothing
inputUncons (Input loc (x:xs)) = Just (x, Input (loc + 1) xs)



data JsonValue 
    = JsonNull
    | JsonBool Bool
    | JsonNum Double
    | JsonStr String
    | JsonArray [JsonValue]
    | JsonObject (Map String JsonValue)
    deriving(Show, Eq)



data ParseError = ParseError Int String
    deriving Show

instance Alternative (Either ParseError) where
    empty = Left $ ParseError 0 "empty"

    Left _ <|> e2 = e2
    e1 <|> _ = e1



newtype Parser a = Parser {
    runParser :: Input -> Either ParseError (Input, a) 
}


instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (input', x) <- p input
            pure (input', f x)


instance Applicative Parser where
    -- pretend that Parser parsed x
    pure x = Parser $ \input -> Right (input, x)

    -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            pure (input'', f a)


instance Alternative Parser where
    -- parser that always fails
    empty = Parser $ const empty

    -- try parser p1 if it fails try parser p2
    Parser p1 <|> Parser p2 = Parser $ \input -> p1 input <|> p2 input


-- parse char if pred holds
parseIf :: String -> (Char -> Bool) -> Parser Char
parseIf descr f = Parser $ \input ->
    case input of
        (inputUncons -> Just (y, ys))
            | f y -> Right (ys, y)
            | otherwise -> Left $ ParseError (inputLoc input) ("Expected " ++ descr ++ ", but found '" ++ [y] ++ "'")

        _ -> Left $ ParseError (inputLoc input) ("Expected " ++ descr ++ ", but reached end of string")


-- split input into part where pred holds and part where it does not
spanP :: String -> (Char -> Bool) -> Parser String
spanP descr = many . parseIf descr


-- whitespace parser
ws :: Parser String
ws = spanP "whitespace" isSpace


-- elements seperated by some seperator example: 1,2,3
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []


-- example "Hello"
stringLiteral :: Parser String
stringLiteral = charP '"' *> many (normalChar <|> escapeChar) <* charP '"'


normalChar :: Parser Char
normalChar = parseIf "non-special char" (\ch -> ch /= '"' && ch /= '\\')


escapeUnicode :: Parser Char
escapeUnicode = chr . fst . head . readHex <$> sequenceA (replicate 4 (parseIf "hex digit" isHexDigit))


escapeChar :: Parser Char
escapeChar = ('"' <$ strP "\\\"") <|>
    ('\\' <$ strP "\\\\") <|>
    ('/' <$ strP "\\/")  <|>
    ('\b' <$ strP "\\b") <|>
    ('\f' <$ strP "\\f") <|>
    ('\n' <$ strP "\\n") <|>
    ('\r' <$ strP "\\r") <|>
    ('\t' <$ strP "\\t") <|>
    (strP "\\u" *> escapeUnicode)


-- parses a single defined char
charP :: Char -> Parser Char
charP c = Parser f
    where
        f input@(inputUncons -> Just (x,xs))
            | x == c    = Right (xs, x)
            | otherwise = Left $ ParseError (inputLoc input) ("Expected '" ++ [c] ++ "', but found '" ++ [x] ++ "'")

        f input = Left $ ParseError (inputLoc input) ("Expected '" ++ [c] ++ "', but reached end of string")


doubleFromParts :: Integer -> Integer -> Double -> Integer -> Double
doubleFromParts sign int dec exp = fromInteger sign * (fromIntegral int + dec) * (10 ^^ exp)


doubleP :: Parser Double
doubleP = doubleFromParts
    <$> (minus <|> pure 1) -- sign
    <*> (read <$> digits)  -- integral part
    <*> ((read <$> (('0':) <$> ((:) <$> charP '.' <*> digits))) <|> pure 0) -- decimal part
    <*> ((e *> ((*) <$> (plus <|> minus <|> pure 1) <*> (read <$> digits))) <|> pure 0) -- exponent
    where
        minus = (-1) <$ charP '-'
        plus = 1 <$ charP '+'
        e = charP 'e' <|> charP 'E'
        digits = some $ parseIf "digit" isDigit


-- parses single defined string
strP :: String -> Parser String
strP = traverse charP


jsonNullParser :: Parser JsonValue
jsonNullParser = JsonNull <$ strP "null"


jsonBoolParser :: Parser JsonValue
jsonBoolParser = t <|> f
    where
        t = JsonBool True <$ strP "true"
        f = JsonBool False <$ strP "false"


jsonStrParser :: Parser JsonValue
jsonStrParser = JsonStr <$> stringLiteral 


jsonNumParser :: Parser JsonValue
jsonNumParser = JsonNum <$> doubleP


jsonArrayParser :: Parser JsonValue
jsonArrayParser = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
    where
        elements = sepBy (ws *> charP ',' <* ws) jsonValueParser


jsonObjectParser :: Parser JsonValue
jsonObjectParser = JsonObject <$>
    (begin *> (fromList <$> manyPairs) <* end)
        where
            begin     = charP '{' *> ws
            end       = ws <* charP '}'
            manyPairs = (sepBy (ws *> charP ',' <* ws) pair)

            pair = (\key _ value -> (key, value))
                <$> stringLiteral                    -- :: Parser (p -> b -> (String, b))
                <*> (ws *> charP ':' <* ws)          -- :: Parser (b -> (String, b))
                <*> jsonValueParser                  -- :: Parser (String, JsonValue)

            {-
                how pair works:

                    0. we start with type: a -> b -> c -> (a, c)

                    1. fmap (a -> b -> c -> (a, c)) into stringLiteral 

                        f = \key _ value -> (key, value)
                        a = result of running stringLiteral

                        giving a Parser that will do the following

                            (rest, value) <- stringLiteral input
                            Just (rest, f a)

                        Note: f a :: b -> c -> (a, c)

                        ==> Parser (b -> c -> (a, c))


                    2. apply result of (ws *> charP ':' <* ws) to   \_ value -> (str, value)

                        pf :: Parser (b -> c -> (a, c))
                        pf = \_ value -> (str, value)

                        pa :: Parser String
                        pa = (ws *> charP ':' <* ws)

                        giving a parser that will do the following:
                            (rest', f) <- pf rest
                            (rest'', spaces) <- pa rest'

                            Just (rest'', f spaces)

                        Note: f spaces :: c -> (a, c)

                        ==> Parser (c -> (a, c))
                    

                    3. apply result of jsonValueParser to           \value -> (str, value)

                        pf :: Parser (c -> (a, c))
                        pf = \value -> (str, value)

                        pa :: Parser JsonValue
                        pa = jsonValueParser

                        giving a parser that will do the following:
                            (rest''', f) <- pf rest''
                            (rest'''', val) <- pa rest'''

                            Just (rest'''', f val)

                        Note: f val :: (a, c)

                        ==> Parser (a, c)
            -}




jsonValueParser :: Parser JsonValue
jsonValueParser = jsonNullParser
    <|> jsonBoolParser
    <|> jsonNumParser
    <|> jsonStrParser
    <|> jsonArrayParser
    <|> jsonObjectParser


jsonParseString :: String -> Either ParseError JsonValue
jsonParseString input = case runParser jsonValueParser $ Input 0 input of
    Left e -> Left e
    Right (_,x) -> Right x


jsonParseFile :: String -> IO (Either ParseError JsonValue)
jsonParseFile path = do
    content <- readFile path
    pure $ jsonParseString content


main = undefined
