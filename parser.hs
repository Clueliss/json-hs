import Data.Char
import Control.Applicative
import Numeric
import System.IO
import Data.Map (Map, insert, fromList)

data JsonValue 
    = JsonNull
    | JsonBool Bool
    | JsonNum Int
    | JsonStr String
    | JsonArray [JsonValue]
    | JsonObject (Map String JsonValue)
    deriving(Show, Eq)

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a) 
}


instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (input', x) <- p input
            Just (input', f x)


instance Applicative Parser where
    -- pretend that Parser parsed x
    pure x = Parser $ \input -> Just (input, x)

    -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    (<*>) (Parser p1) (Parser p2) =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)


instance Alternative Parser where
    -- parser that always fails
    empty = Parser $ \_ -> Nothing

    -- try parser p1 if it fails try parser p2
    (<|>) (Parser p1) (Parser p2) = Parser $ \input -> p1 input <|> p2 input


-- parse char if pred holds
parseIf :: (Char -> Bool) -> Parser Char
parseIf f = Parser g 
    where
        g (x:xs) = if f x
            then Just (xs, x)
            else Nothing
        g [] = Nothing


-- split input into part where pred holds and part where it does not
spanP :: (Char -> Bool) -> Parser String
spanP = many . parseIf


-- whitespace parser
ws :: Parser String
ws = spanP isSpace


-- elements seperated by some seperator example: 1,2,3
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []


-- example "Hello"
stringLiteral :: Parser String
stringLiteral = charP '"' *> many (normalChar <|> escapeChar) <* charP '"'


normalChar :: Parser Char
normalChar = parseIf (\ch -> ch /= '"' && ch /= '\\')


escapeUnicode :: Parser Char
escapeUnicode = chr . fst . head . readHex <$> sequenceA (replicate 4 (parseIf isHexDigit))


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
        f (x:xs) = if x == c
            then Just (xs, x)
            else Nothing 
        f [] = Nothing


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
jsonNumParser = Parser (f 0)
    where
        digitAdd = \acc cur -> (acc * 10) + cur

        f :: Int -> String -> Maybe (String, JsonValue)
        f _ [] = Nothing
        f acc input = let 
            (num, rest) = (span isDigit input) in
                case map ((+) (- 48) . ord) num of
                    [] -> Nothing
                    list -> Just (rest, JsonNum (foldl digitAdd 0 list))


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


jsonParseFile :: String -> IO (Maybe (String, JsonValue))
jsonParseFile path = do
    content <- readFile path
    return (runParser jsonValueParser content)


main = undefined
