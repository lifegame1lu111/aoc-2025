{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromJust)
import Data.Bifunctor (bimap)
import Data.List (transpose, unsnoc)
import Data.Function ((&))
import Data.List.Split (splitOn)

type Op = (Int, Int -> Int -> Int)
type Problem = ([Int], Op)

parseOp :: T.Text -> Op
parseOp = \case
    "+" -> (0, (+))
    _ -> (1, (*))

parse1 :: T.Text -> [Problem]
parse1 = map (bimap (map (read . T.unpack)) parseOp . fromJust . unsnoc)
    . transpose
    . map (filter (not . T.null) . T.splitOn " ")
    . T.lines

part1 :: [Problem] -> Int
part1 = \case
    [] -> 0
    ((nums, (init, op)) : ps) -> foldl op init nums + part1 ps

parse2 :: T.Text -> [Problem]
parse2 content =
    let (nums, ops) = fromJust . unsnoc . T.lines $ content
        nums' :: [[Int]]
        nums' = nums
            & map (T.unpack . T.replace " " "0")
            & transpose
            & map (read . filterZeros)
            & splitOn [0]
        ops' = ops
            & T.splitOn " "
            & filter (not . T.null)
            & map parseOp
    in zip nums' ops'
  where
    filterZeros :: String -> String
    filterZeros num
       | all (== '0') num = "0"
       | otherwise = filter (/= '0') num

part2 :: [Problem] -> Int
part2 = part1

main :: IO ()
main = do
    content <- TIO.readFile "input06.txt"

    let parsed = parse1 content
        result1 = part1 parsed
        parsed' = parse2 content
        result2 = part2 parsed'

    TIO.putStrLn $ "Part 1: " <> T.show result1
    TIO.putStrLn $ "Part 2: " <> T.show result2
