module Board(Board(..),update) where 

import Data.List ((\\))

data Board = Board [Int] deriving (Eq, Show)

isEmpty :: Board -> Bool
isEmpty (Board []) = True
isEmpty _ = False

update :: Int -> [Int] -> Board -> Board
update target replacement (Board b) = Board ((b \\ [target]) ++ replacement)
    
clearBoard :: (Board -> (Int,[Int])) -> Board -> Int
clearBoard = clearRec 0
    where
      clearRec count strategy board = 
          if isEmpty board
              then count
              else 
                  let (target,replacement) = strategy board in
                  clearRec (count+1) strategy (update target replacement board)

-- Replace maximal number n by [1..n-1]
maxnum_strategy :: Board -> (Int,[Int])
maxnum_strategy (Board b) = 
    let n = maximum b in
    (n,[1..n-1])

main = do
    let board = Board [10]
    let numIterations = clearBoard maxnum_strategy board 
    putStrLn $ (show numIterations) ++ " iterations needed to clear " ++ show board 