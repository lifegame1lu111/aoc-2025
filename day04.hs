import Data.Maybe (isJust, mapMaybe)
import Data.List ((!?))
import Data.Bifunctor (bimap)
import Data.Function ((&))

type Grid = [String]
type Point = (Int, Int)

getGridSize :: Grid -> (Int, Int)
getGridSize grid@(row : _) = (length row, length grid)
getGridSize _ = (0, 0)

part1 :: (Int, Int) -> Grid -> [Point]
part1 (xMax, yMax) grid = [(getAdjRollCount (x, y), (x, y), (grid !! y) !! x) 
                          | x <- [0..xMax - 1]
                          , y <- [0..yMax - 1]
                          ]
    & filter (\(count, _, tile) -> count < 4 && tile == '@')
    & map (\(_, coord, tile) -> coord)
  where
    getAdjRollCount :: Point -> Int
    getAdjRollCount (x, y) = deltas
        & mapMaybe (tryGetTile . bimap (x +) (y +))
        & filter (== '@')
        & length

    deltas :: [Point]
    deltas = [ (0, 1)
             , (0, -1)
             , (1, 0)
             , (-1, 0)
             , (1, 1)
             , (-1, 1)
             , (1, -1)
             , (-1, -1)
             ]

    tryGetTile :: Point -> Maybe Char
    tryGetTile (x, y) = case grid !? y of
        Just row -> row !? x
        Nothing -> Nothing

part2 :: (Int, Int) -> Grid -> Int
part2 size@(xMax, yMax) grid = case part1 size grid of
    [] -> 0
    points -> let go :: Point -> Char -> Char
                  go point tile = if point `elem` points
                                  then '.'
                                  else tile
                  grid' = [ [ go (x, y) $ (grid !! y) !! x
                            | x <- [0..xMax - 1]
                            ]
                          | y <- [0..yMax - 1]
                          ]
              in length points + part2 size grid'
    
main :: IO ()
main = do
    grid <- lines <$> readFile "input04.txt"

    let size = getGridSize $! grid
        result1 = length $ part1 size grid
        result2 = part2 size grid

    putStrLn $ "Part 1: " <> show result1
    putStrLn $ "Part 2: " <> show result2
