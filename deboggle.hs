{-# LANGUAGE TemplateHaskell #-}

import Text.Regex.Posix
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Utils as List
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as Char8
import Data.FileEmbed
import Data.Time
import System.IO
import System.Exit
import Control.DeepSeq


-- (rows, size)
-- rows :: [String]
-- size :: Int
type Board = ([String], Int)

-- (x, y)
-- x :: Int
-- y :: Int
type Point = (Int, Int)

-- (points, value)
-- points :: [Point]
-- value :: String
---- Note: the `Point`s in `points` are arranged from last to first due to
----  greater efficiency of `:` over `++` for adding `Point`s
type Path  = ([Point], String)


-- propmts the user for text input with the message `msg`
prompt :: String -> IO String
prompt msg = do putStr msg
                hFlush stdout
                getLine

-- get all words that contain only the letters in `board` [not count-sensitive] and whose length is
--  less then or equal to the number of cells in board, i.e. `size` ^ 2
getWords :: Board -> Set.Set String
getWords (board, size) = let w = Char8.unpack $(embedFile "words.txt")
                             letters = Set.fromList $ foldl1 (++) $ board
                         in Set.fromList $ filter (\s -> length s >= 4 && length s <= size ^ 2 && Set.fromList s `Set.isSubsetOf` letters) $ map (List.replace "qu" "_") $ words w

-- all the prefixes of `s` of length >2
-- Ex: `prefixes "apple"` evaluates to ["ap","app","appl","apple"]
-- allows paths to be abandoned if they cannot possibly contain any words (i.e. no words start with that path's value)
prefixes :: String -> Set.Set String
prefixes s = Set.fromList [take n s | n <- [2..length s]]

-- all the points (x', y') adjacent to (x, y) such that 0 <= x' <= m, 0 <= y' <= m for all (x', y') (i.e. only points on
--  an m*m grid)
adjacent :: Int -> Point -> [Point]
adjacent m (x, y) = [(x', y') | x' <- xs, y' <- ys, x' /= x || y' /= y]
                    where xs = [max 0 (x-1) .. min (m-1) (x+1)]
                          ys = [max 0 (y-1) .. min (m-1) (y+1)]

-- gets n rows of letters, ensuring the length of each row = size
getRows :: Int -> Int -> IO [String]
getRows _ 0 = return []
getRows size n = do let rowNum = show $ size - n + 1
                    rown' <- prompt $ 'R' : rowNum ++ ": "
                    let rown = map Char.toLower rown'
                    if not $ rown =~ "^[a-z]*$"
                       then die "Error: board must only contain the letters a-z." else return '\NUL'
                    if length rown < size
                       then die $ "Error: Error: incorrect size for R" ++ rowNum ++ "." else return '\NUL'
                    rows <- getRows size $ n - 1
                    return $ List.replace "q" "_" rown : rows

-- gets the Boggle board from the user
-- the first row determines the size of the board, `size`, and the remaining rows are gotten
--  by `getRows size (size - 1)`
getBoard :: IO Board
getBoard = do putStrLn "Enter the letters of the board, row by row, without spaces (for \"Qu\" enter just \"Q\":"
              row1' <- prompt "R1: "
              let row1 = map Char.toLower row1'
              let size = length row1
              if not $ row1 =~ "^[a-z]*$"
                 then die "Error: board must only contain the letters a-z." else return '\NUL'
              if length row1 < 2
                 then die "Error: board must be at least 2x2." else return '\NUL'
              rows <- getRows size $ size - 1
              return (List.replace "q" "_" row1 : rows, length row1)

-- the letter in `board` at point (x, y)
(#) :: Board -> Point -> Char
(board, _) # (x, y) = board !! y !! x

-- all the branches of `path` by extending it along all valid points adjacent to its top point
branches :: Board -> Path -> [Path]
branches board@(_, size) (path, str) = [(p : path, str ++ [board # p]) | p <- adjacent size $ head path, not $ p `elem` path]

-- continues the search along all `branches` of path
-- aborts paths which cannot contain any words (see `prefixes`)
extendPath :: Board -> Set.Set String -> Set.Set String -> Path -> [String]
extendPath board wds pfxs path@(_, str)
  | length str > 2 &&
    not (str `Set.member` pfxs) = []
  | str `Set.member` wds        = List.replace "_" "qu" str : concat [extendPath board wds pfxs path' | path' <- branches board path]
  | otherwise                   = concat [extendPath board wds pfxs path' | path' <- branches board path]

-- prints a each element in a list of `String`s on its own line
printAll :: [String] -> IO ()
printAll [] = return ()
printAll (x:xs) = do putStrLn x
                     printAll xs

main = do board <- getBoard
          start <- getCurrentTime
          let wds = getWords board
          let pfxs = Set.unions $ map prefixes $ Set.elems wds
          let res = concat [extendPath board wds pfxs ([(x, y)], [board # (x, y)]) | x <- [0..snd board - 1], y <- [0.. snd board - 1]]
          -- sort `res` alphabetically, then by length (result will be sorted primarily by size)
          let sortedRes = (List.sortBy (\x y -> length y `compare` length x)) . List.sort $ res
          -- use `deepseq` to force complete evaluation, for accurate time logging
          end <- sortedRes `deepseq` getCurrentTime
          putStrLn $ (show . length) sortedRes ++ " word(s) found in " ++ (init . show) (diffUTCTime end start) ++ " seconds"
          printAll sortedRes