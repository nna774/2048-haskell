module Game.Board(Board, Grid, SessionID, Direction, allDirection, start) where

import Network.HTTP
import Network.Browser (browse, request, request)
-- import Control.Monad
-- import Control.Applicative

-- {"grid":[[0,0,0,0],[2,0,0,0],[0,0,0,2],[0,0,0,0]],"score":0,"points":0,"moved":false,"over":false,"won":false,"session_id":"a616e9723d31660f4ac95abafa6d54ff27cd5a5a","zen":"We are afraid of truth, afraid of fortune, afraid of death, and afraid of each other."}

data Grid = Grid ( (Int, Int, Int, Int)
                 , (Int, Int, Int, Int)
                 , (Int, Int, Int, Int)
                 , (Int, Int, Int, Int)
                 ) deriving(Show, Read, Eq, Ord)
type SessionID = String

data Direction = UP | RIGHT | DOWN | LEFT deriving(Show, Read, Eq, Ord, Enum)

type Board = ( Grid
             , SessionID
             , Int -- Score
             , Bool -- moved
             , Bool -- Over
             )

allDirection :: [Direction]
allDirection = [UP .. LEFT]

splitBy :: Char -> String -> [String]
splitBy c str  =  cons (case break (== c) str of
                             (l, s') -> (l, case s' of
                                              []      -> []
                                              _:s''   -> splitBy c s''))
    where
      cons ~(h, t)        =  h : t

start :: String -> IO Board
start endpoint = do -- ヤバい
  let uri = endpoint ++ "/hi/start/json"
  (_, res) <- browse $ request $ getRequest uri
  let ls = splitBy ':' $ rspBody res
  let grid = (read :: String ->  [[Int]]) $ take 41 $ ls !! 1
  let grid' = Grid $ 
              ( (grid!!0!!0, grid!!0!!1, grid!!0!!2, grid!!0!!3)
              , (grid!!1!!0, grid!!1!!1, grid!!1!!2, grid!!1!!3)
              , (grid!!2!!0, grid!!2!!1, grid!!2!!2, grid!!2!!3)
              , (grid!!3!!0, grid!!3!!1, grid!!3!!2, grid!!3!!3)
              )
  let sid = (read :: String -> String) $ takeWhile (/=',') $ ls !! 7
  return (grid', sid, 0, False, False)

move :: SessionID -> Direction -> IO Board
move = undefined
