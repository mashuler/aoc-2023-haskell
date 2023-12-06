import Data.Char (isDigit)
import Data.List (find)

main :: IO ()
main = do
    putStrLn "Day 1"

    putStrLn "Part 1"
    contents <- getInput "input.txt"
    print $ part1 contents

part1 :: [String] -> Int
part1 xs = sum $ map part1' xs

part1' :: String -> Int
part1' str = read [first, last]
    where
        first = firstNum str
        last = lastNum str

part2 :: [String] -> Int
part2 _ = 0

firstNum :: String -> Char
firstNum str = case find isDigit str of
    Just c  -> c
    Nothing -> error "No number present in string!"

lastNum :: String -> Char
lastNum str = case find isDigit (reverse str) of
    Just c  -> c
    Nothing -> error "No number present in string!"

getInput :: String -> IO [String]
getInput filename = do
    contents <- readFile filename
    return (lines contents)
