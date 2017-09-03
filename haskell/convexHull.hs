import Text.Printf

import           Data.List
import           Prelude   hiding (Left, Right)

data Point = Point {
  x :: Float,
  y :: Float}
  deriving Show

data Direction = Left | Right | Straight
  deriving (Eq, Show)

crossProduct :: Point -> Point -> Point -> Float
crossProduct (Point x1 y1) (Point x2 y2) (Point x3 y3) =
  let dx1 = x2 - x1
      dy1 = y2 - y1
      dx2 = x3 - x1
      dy2 = y3 - y1
   in
    dx1 * dy2 - dx2 * dy1

turnDirection :: Point -> Point -> Point -> Direction
turnDirection a b c
  | cross < 0 = Left
  | cross == 0 = Straight
  | cross > 0 = Right
  where cross = crossProduct a b c


sequentialDirections :: [Point] -> [Direction]
sequentialDirections [] = []
sequentialDirections [_, _] = []
sequentialDirections (a:b:c:xs) = (turnDirection a b c) : (sequentialDirections (b:c:xs))

orderOfLessPredicate :: (a -> a -> Bool) -> a -> a -> Ordering
orderOfLessPredicate pred x y
  | (pred x y) = LT
  | (pred y x) = GT
  | otherwise = EQ


grahamScan :: [Point] -> [Point]
grahamScan rawPointList =
  let (minPoint:pointList) =
        sortBy
        (orderOfLessPredicate
         (\ pointA pointB -> (y pointA) < (y pointB) ||
           ((y pointA) == (y pointB) && (x pointA) < (x pointB)))
        )
        rawPointList
  in
      let orderedPointList =
            sortBy
            (\ pointA pointB ->
               (case (turnDirection minPoint pointA pointB) of
                  {Left -> LT; Straight -> EQ; Right -> GT})
            )
            pointList
     in
        minPoint : (grahamScanImpl orderedPointList [])
        where
          grahamScanImpl [] stack = stack
          grahamScanImpl (head:tail) [] = grahamScanImpl tail [head]
          grahamScanImpl (head:tail) stack@(top:[]) = grahamScanImpl tail (head:stack)
          grahamScanImpl points@(head:tail) stack@(top:before:rest)
            | (turnDirection before top head) == Left = grahamScanImpl tail (head:stack)
            | otherwise = grahamScanImpl points (before:rest)


lineLength :: Point -> Point -> Float
lineLength pointA pointB =
  let dx = (x pointB) - (x pointA); dy = (y pointB) - (y pointA) in sqrt ((dx * dx) + (dy * dy))

perimeter :: [Point] -> Float
perimeter []       = 0
perimeter (x:[])   = 0
perimeter (x:y:xs) = (lineLength x y) + (perimeter (y:xs))

listWithHeadAtEnd :: [a] -> [a]
listWithHeadAtEnd []          = []
listWithHeadAtEnd list@(x:xs) = list ++ [x]

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let
    points = map (\[x, y] -> (Point x y)). map (map (read::String->Float)). map words. lines $ content
    convexHull = grahamScan points
    ans = (perimeter (listWithHeadAtEnd convexHull))
  printf "%.1f\n" ans

