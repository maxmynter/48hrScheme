import Numeric (readFloat, readOct)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | Number Integer
  | Float Double
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
    Just _ -> fail "Unknown number prefix"
    Nothing -> parseDecimal <|> parseFloat

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
  headElement <- endBy parseExpr spaces
  tailElement <- char '.' >> spaces >> parseExpr
  return $ DottedList headElement tailElement

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number content) = show content
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Float f) = show f
showVal (Character n) = show n
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList heads tails) = "(" ++ unwordsList heads ++ " . " ++ showVal tails ++ ")"

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
   in if null parsed
        then 0
        else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Atom _) = val
eval val@(Character _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
