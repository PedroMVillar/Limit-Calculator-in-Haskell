import Text.ParserCombinators.Parsec -- (Parser, ParseError, parse, digit, char, many1, option, sepBy, (<|>), try)

data Term = Term Double Int deriving Show -- coefficient, exponent
data Polynomial = Polynomial [Term] deriving Show -- list of terms

parsePolynomial :: String -> Either ParseError Polynomial -- parse polynomial from string
parsePolynomial input = parse polynomialParser "" input -- parse polynomial from string

polynomialParser :: Parser Polynomial 
polynomialParser = do
  terms <- sepBy termParser (skipMany space >> (char '+' <|> char '-') >> skipMany space) -- parse terms separated by '+' or '-'
  return $ Polynomial terms -- return polynomial

termParser :: Parser Term
termParser = try termWithX <|> termWithoutX
  where
    termWithX = do
      sign <- option '+' (skipMany space >> (char '-' <|> char '+'))
      coefficient <- option "1" (many1 (digit <|> char '.')) -- parse coefficient
      skipMany space
      char 'x' -- parse 'x'
      skipMany space
      exponent <- option "1" (char '^' >> many1 digit) -- parse exponent
      let signedCoefficient = if sign == '-' then read ("-" ++ coefficient) else read coefficient
      return $ Term (signedCoefficient :: Double) (read exponent :: Int) -- return term
    termWithoutX = do
      sign <- option '+' (skipMany space >> (char '-' <|> char '+'))
      coefficient <- many1 (digit <|> char '.')
      let signedCoefficient = if sign == '-' then read ("-" ++ coefficient) else read coefficient
      return $ Term (signedCoefficient :: Double) 0

calculateLimit :: Polynomial -> Double -> Double -- calculate limit
calculateLimit (Polynomial terms) x = sum $ map (\(Term coefficient exponent) -> coefficient * (x ** fromIntegral exponent)) terms -- calculate limit

main :: IO ()
main = do
  putStrLn "Enter a polynomial:" -- prompt user for polynomial
  input <- getLine -- read input
  let polynomial = parsePolynomial input -- parse polynomial
  case polynomial of -- handle result
    Left error -> print error -- print error
    Right polynomial -> do -- calculate limit
      putStrLn "Enter a value for x:" -- prompt user for x
      x <- readLn -- read x
      print $ calculateLimit polynomial x -- print limit