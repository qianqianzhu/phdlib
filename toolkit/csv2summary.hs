import CSVParser
import Data.List
import Data.Text (strip, pack, unpack, toLower)

countFreq :: [String] -> [(String, Int)]
countFreq list = map (\x -> (head x, length x)) . group . sort $ list

splitBy :: (Char -> Bool) -> String -> [String]
splitBy p s = case dropWhile p s of
                    "" -> []
                    s' -> w : splitBy p s''
                           where
                             w = unpack $ toLower . strip $ pack w'
                             (w',s'') = break p s'
sortPair :: [(String,Int)] -> [(String,Int)]
sortPair [] = []
sortPair (x:xs) =
  let smaller = sortPair [a | a<-xs, snd a <= snd x]
      bigger = sortPair [a | a<-xs, snd a > snd x]
  in bigger ++ [x] ++ smaller

toLatex :: [(String,Int)] -> String
toLatex [] = []
toLatex (x:xs) = "  " ++ attr ++ " & " ++ show num ++ "\\\\\n  \\hline\n" ++ toLatex xs
                  where (attr, num) = x

flatten :: [String] -> [String]
flatten [] = []
flatten (x:xs) = case findIndex (==',') x of
                    Nothing -> [unpack $ toLower . strip $ pack x] ++ flatten xs
                    Just n -> splitBy (==',') x ++ flatten xs
main =
  do c <- getContents
     case parseCSV c of
       Left e -> do putStrLn "Error parsing input:"
                    print e
       Right r -> mapM_ putStrLn $ map (\x -> toLatex $ sortPair $ countFreq $ flatten x) $ tail $ transpose r
