import Data.List

containTitle::String -> Bool
containTitle x = isInfixOf "Author" x

extract::String->String
extract [] = []
extract ('{':xs) = extract (reverse xs)
extract ('}':xs) = reverse xs
extract (x:xs) = extract xs

extractFirstAuthor::String->String
extractFirstAuthor [] = []
extractFirstAuthor ('{':xs) = extractFirstAuthor (reverse xs)
extractFirstAuthor (',':xs) = reverse xs
extractFirstAuthor (x:xs) = extractFirstAuthor xs

extractSecondAuthor::String->String
extractSecondAuthor [] = []
extractSecondAuthor ('a':'n':'d':' ':xs) = extractSecondAuthor (reverse xs)
extractSecondAuthor (',':xs) = reverse xs
extractSecondAuthor (x:xs) = extractSecondAuthor xs

getAuthor::[String]->[String]
getAuthor content = [extract titleLine | titleLine <- content, containTitle titleLine]

countAuthor::String -> Int
countAuthor = length . filter (==',')

genAuthor x
  | count > 2  = take (head commaIndices) x ++ " et al."
  | count == 2 = take (head commaIndices) x ++ " and" ++ drop secondB (take secondE x)
  | otherwise = take (head commaIndices) x
   where commaIndices = findIndices (\x -> x==',') x
         spaceIndices = findIndices (\x -> x==' ') x
         secondB = head $ reverse [x | x<- spaceIndices, x < head (tail commaIndices)]
         secondE = head (tail commaIndices)
         count = length(commaIndices)

main = interact $ unlines . map (\x -> genAuthor x) . getAuthor . lines