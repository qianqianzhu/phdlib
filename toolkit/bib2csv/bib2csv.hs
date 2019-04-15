import Data.List

containTitle::String -> Bool
containTitle x = isInfixOf "Title" x

extract::String->String
extract [] = []
extract ('{':xs) = extract (reverse xs)
extract ('}':xs) = reverse xs
extract (x:xs) = extract xs

getTitle::[String]->[String]
getTitle content = [extract titleLine | titleLine <- content, containTitle titleLine]

main = interact $ unlines . getTitle . lines
