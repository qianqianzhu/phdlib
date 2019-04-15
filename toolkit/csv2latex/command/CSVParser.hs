module CSVParser(
  parseCSV,
  csvFile,
  parse
  ) where
import Text.ParserCombinators.Parsec
import Data.List

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell =
  do char '"'
     content <- many quotedChar
     char '"' <?> "quote at the end of cell"
     return content

quotedChar =
  noneOf "\""
  <|> try (string "\"\"" >> return '"')

eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main =
  do c <- getContents
     case parseCSV c of
       Left e -> do putStrLn "Error parsing input:"
                    print e
       Right r -> mapM_ print r
