{-# LANGUAGE TemplateHaskell #-}

import Text.Regex.Posix
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Utils as List
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as Char8
import Data.Function
import Data.FileEmbed
import Data.Time
import System.IO
import System.Exit
import Control.DeepSeq


data Board = Board { rows :: [String]
                   , size :: Int
                   }

-- the letter in `board` at point (x, y)
(#) :: Board -> Point -> Char
(Board rows _) # (Point x y) = rows !! y !! x

data Point = Point Int Int deriving (Eq)

---- Note: the `Point`s in `points` are arranged from last to first due to
----  greater efficiency of `:` over `++` for appending `Point`s
data Path = Path { points :: [Point]
                 , value :: String
                 }


-- propmts the user for text input with the message `msg`
prompt :: String -> IO String
prompt msg = do putStr msg
                hFlush stdout
                getLine

-- runs `act` iff `cond` is True
iff :: Bool -> IO a -> IO ()
iff cond act = if cond then act >> return () else return ()

-- get all words that contain only the letters in `board` [not count-sensitive] and whose length is
--  less then or equal to the number of cells in board, i.e. `size` ^ 2
getWords :: Board -> Set.Set String
getWords (Board rows size) = let w = Char8.unpack $(embedFile "words.txt")
                                 letters = Set.fromList . concat $ rows
                             in  Set.fromList [List.replace "qu" "_" word | word <- words w
                                              , length word >= 4, length word <= size ^ 2, (Set.fromList word) `Set.isSubsetOf` letters]

-- all the prefixes of `s` of length >2
-- Ex: `prefixes "apple"` evaluates to ["ap","app","appl","apple"]
-- allows paths to be abandoned if they cannot possibly contain any words (i.e. no words start with that path's value)
prefixes :: String -> Set.Set String
prefixes s = Set.fromList [take n s | n <- [2..length s]]

-- all the points (x', y') adjacent to (x, y) such that 0 <= x' <= m, 0 <= y' <= m for all (x', y') (i.e. only points on
--  an m*m grid)
adjacent :: Int -> Point -> [Point]
adjacent m (Point x y) = let xs = [max 0 (x-1) .. min (m-1) (x+1)]
                             ys = [max 0 (y-1) .. min (m-1) (y+1)]
                         in  [Point x' y' | x' <- xs, y' <- ys, x' /= x || y' /= y]

-- gets n rows of letters, ensuring the length of each row = size
getRows :: Int -> Int -> IO [String]
getRows _ 0 = return []
getRows size n = do let rowNum = show $ size - n + 1
                    rown' <- prompt $ 'R' : rowNum ++ ": "
                    let rown = map Char.toLower rown'
                    iff (not $ rown =~ "^[a-z]*$") $ die "Error: board must only contain the letters a-z."
                    iff (length rown < size) $ die $ "Error: Error: incorrect size for R" ++ rowNum ++ "."
                    rows <- getRows size $ n - 1
                    return $ List.replace "q" "_" rown : rows

-- gets the Boggle board from the user
-- the first row determines the size of the board, `size`, and the remaining rows are gotten
--  by `getRows size (size - 1)`
getBoard :: IO Board
getBoard = do putStrLn "Enter the letters of the board, row by row, without spaces (for \"Qu\" enter just \"Q\"):"
              row1' <- prompt "R1: "
              let row1 = map Char.toLower row1'
              let size = length row1
              iff (not $ row1 =~ "^[a-z]*$") $ die "Error: board must only contain the letters a-z."
              iff (length row1 < 2) $ die "Error: board must be at least 2x2."
              rows <- getRows size $ size - 1
              return $ Board (List.replace "q" "_" row1 : rows) (length row1)

-- all the branches of `path` by extending it along all valid points adjacent to its top point
branches :: Board -> Path -> [Path]
branches board@(Board _ size) (Path points str) = [Path (p : points) (str ++ [board # p]) | p <- adjacent size $ head points, not $ p `elem` points]

-- continues the search along all `branches` of path
-- aborts paths which cannot contain any words (see `prefixes`)
extendPath :: Board -> Set.Set String -> Set.Set String -> Path -> [String]
extendPath board wds pfxs path@(Path _ str)
  | length str > 2 &&
    not (str `Set.member` pfxs) = []
  | str `Set.member` wds        = List.replace "_" "qu" str : concat [extendPath board wds pfxs path' | path' <- branches board path]
  | otherwise                   = concat [extendPath board wds pfxs path' | path' <- branches board path]

-- prints a each element in a list of `String`s on its own line
printAll :: [String] -> IO ()
printAll [] = return ()
printAll (x:xs) = do putStrLn x
                     printAll xs

main = do board@(Board _ size) <- getBoard
          start <- getCurrentTime
          let wds = getWords board
          let pfxs = Set.unions $ map prefixes $ Set.elems wds
          let res = concat [extendPath board wds pfxs (Path [Point x y] [board # Point x y]) | x <- [0..size - 1], y <- [0..size - 1]]
          -- sort `res` alphabetically, then by length (result will be sorted primarily by length)
          let sortedRes = List.sortBy (compare `on` length) . List.sort $ res
          -- use `deepseq` to force complete evaluation, for accurate time logging
          end <- sortedRes `deepseq` getCurrentTime
          putStrLn $ (show $ length sortedRes) ++ " word(s) found in " ++ (init . show $ diffUTCTime end start) ++ " seconds"
          printAll sortedRes