
import System.Environment

import System.IO
import Data.List.Split
import Data.List

main :: IO()
main = getArgs >>= mapM_ execute


execute:: String -> IO()
execute filepath = do
    content <- openFile filepath ReadMode >>= hGetContents
    let flines = filter (not . null ) (splitOn "\n" content)

    --let t = read $ flines !! 0 :: Integer
    mapM_ putStrLn flines

    let res = [show $ parseBees z | z <- (drop 1 flines)]
    putStrLn ""
    mapM_ putStrLn res

    writeFile "answer1.txt" $ intercalate "\n" res


parseBees :: String -> Double
parseBees information =
  case length content of
    3 -> solveBees $ content ++ [1]
    _ -> -2.0
  where
    info = splitOn " " information
    content = [read x :: Double | x <- info]



solveBees :: [Double] -> Double
solveBees parameters =
  case or [n1 == n2, n2 > 10e5, i > 50000] of
    True -> convertOutput n2
    False -> solveBees [n2, a, b, i + 1]
  where
   [n1, a, b, i] = parameters
   n2 = max 0 ((a * n1) - (b * (n1 * n1)))


convertOutput :: Double -> Double
convertOutput inp =
  case inp > 10e5 of
    True -> -1
    False -> case inp < 10e-4 of
      True -> 0
      False -> inp
