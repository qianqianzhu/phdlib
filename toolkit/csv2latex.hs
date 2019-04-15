import CSVParser

csv2latex :: [[String]] -> [String]
csv2latex [] = []
csv2latex (row:rest) = [formatLatex row] ++ csv2latex rest

formatLatex :: [String] -> String
formatLatex [] = []
formatLatex [x] = x ++ "\\\\\n\\hline"
formatLatex (x:xs) = x ++ " & " ++ formatLatex xs

main =
  do c <- getContents
     case parseCSV c of
       Left e -> do putStrLn "Error parsing input:"
                    print e
       Right r -> mapM_ putStrLn $ csv2latex r
