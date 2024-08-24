import Numeric (readOct)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | Number Integer
  | Character Char
  | String String
  | Bool Bool
  | List [LispVal]
  | DottedList [LispVal] LispVal

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many stringChar
  _ <- char '"'
  return $ String x
  where
    stringChar = escapedChar <|> noneOf "\""
    escapedChar = do
      _ <- char '\\'
      c <- oneOf "\"\\nrt"
      return $ case c of
        '"' -> '"'
        '\\' -> '\\'
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> c

parseCharacter :: Parser LispVal
parseCharacter = do
  _ <- string "#\\"
  value <- parseCharName <|> anyChar
  return $ Character value

parseCharName :: Parser Char
parseCharName = do
  name <- string "space" <|> string "newline"
  return $ case name of
    "space" -> ' '
    "newline" -> '\n'
    _ -> error $ "Unkown character name: " ++ name

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  _ <- char '.'
  y <- many1 digit
  return $ Float (fst . head . readFloat $ x ++ "." ++ y)

parseNumber :: Parser LispVal
parseNumber = do
  maybePrefix <- optionMaybe (try (string "#" >> oneOf "bdox"))
  case maybePrefix of
    Just 'b' -> parseBinary
    Just 'o' -> parseOctal
    Just 'x' -> parseHexadecimal
    Nothing -> parseDecimal

binToDec :: String -> Integer
binToDec = foldl (\acc x -> acc * 2 + if x == '0' then 0 else 1) 0

parseBinary :: Parser LispVal
parseBinary = do
  digits <- many1 (oneOf "01")
  return $ Number $ binToDec digits

parseOctal :: Parser LispVal
parseOctal = do
  digits <- many1 octDigit
  return $ Number $ fst $ head $ readOct digits

parseHexadecimal :: Parser LispVal
parseHexadecimal = do
  digits <- many1 hexDigit
  return $ Number $ fst $ head $ readOct digits

parseDecimal :: Parser LispVal
parseDecimal = Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseCharacter
    <|> parseQuoted
    <|> do
      _ <- char '('
      x <- try parseList <|> parseDottedList
      _ <- char ')'
      return x

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

readExpr :: String -> String
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
