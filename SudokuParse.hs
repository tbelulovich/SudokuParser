module SudokuParse where

import Graphics.GD
import Data.List
import Data.Array
import Control.Applicative
import qualified Data.Set as S
import Control.Monad.State
import System.Process
import Control.Monad.Trans.Maybe
import Puzzle.List
import Data.Ord

type Grid = [[Cell]]
data Cell = Cell

type Component = S.Set Point
type PixelMap = Array Point RGBA
type RGBA = (Int,Int,Int,Int)

-- | Input image location
imgloc = "test2.png"

getPixelRGBA at img = toRGBA <$> getPixel at img

-- | Compute the component of a given point
floodFillFrom :: Point -> PixelMap -> Component
floodFillFrom i arr = execState (search i) S.empty where
  search i = do
    j <- get
    if (not $ S.member i j)
      then do     
        put $ S.insert i j
        let good p = (not $ S.member p j) && inRange (bounds arr) p && (arr ! p == arr ! i)
        let neighbors = filter good [(fst i + dx, snd i + dy) | dx <- [-1,0,1]
                                                              , dy <- [-1,0,1]
                                                              , (dx /= 0 || dy /= 0)]
        mapM_ search neighbors
 
           
      else return ()

-- | Lift a function defined on points to components.
-- it is assumed that the function has the same output on all points within a component.
queryComponent :: Component -> (Point -> a) -> a
queryComponent c f = f $ head $ S.toList c

floodFill :: PixelMap -> [Component]
floodFill arr = snd $ execState search ((S.fromList $ range $ bounds arr),[])
  where
    unvisited = fst <$> get
    addComponent c = do
      (s,r) <- get
      put (S.difference s c, c:r)

    search = do
      unseen <- unvisited
      unless (S.null unseen) $ do
          let k = head $ S.toList unseen
          let c = floodFillFrom k arr
          addComponent c
          search

cropImage :: Image -> Point -> Point -> IO Image
cropImage img (ulx,uly) (lrx,lry) = do
  let dx = lrx-ulx
      dy = lry-uly
  g <- newImage (dx,dy)
  copyRegion (ulx,uly) (dx,dy) img (0,0) g
  return g


boundingBox :: Component -> (Int,Int,Int,Int)
boundingBox c = (ulx,uly,lrx,lry) where
  l = S.toList c
  ulx = minimum $ map fst l
  uly = minimum $ map snd l
  lrx = maximum $ map fst l
  lry = maximum $ map snd l

-- | Crop to a *rectangular* component.
cropToCell :: Image -> Component -> IO Image
cropToCell img c = cropImage img (ulx,uly) (lrx,lry) where
  (ulx,uly,lrx,lry) = boundingBox c

-- | Get all the pixels for an image.                      
pixels :: Image -> IO PixelMap
pixels img = do
  k <- imageSize img
  values <- mapM (flip getPixelRGBA img) (range ((0,0),k))
  return $ listArray ((0,0),k) values

-- | Fill a component
setComponent :: Component -> RGBA -> Image -> IO ()
setComponent ps c i =
  let l  = S.toList ps
      (c1,c2,c3,c4) = c
      c' = rgba c1 c2 c3 c4
  in
   mapM_ (\r -> setPixel r c' i) l

-- | Compute the outermost (first from upper left corner) black component
borderGrid :: Image -> IO Image
borderGrid i = do
  dims <- imageSize i
  m <- pixels i
  out <- newImage dims
  let fb = (x,x) where x = head $ filter (\t -> m ! (t,t) == (0,0,0,0)) [1 .. 1000] -- HACK: 1000 "magic #"
  let ps = floodFillFrom fb m
  fillImage (rgba 255 255 255 0) out
  setComponent ps (0,0,0,0) out
  return out

-- | Get all the cells out of a Sudoku grid image
computeCells :: Image -> IO [Image]
computeCells i = do
  g <- borderGrid i
  px <- pixels g
  let isWhite x = queryComponent x (\y -> px ! y) == (255,255,255,0)
      isInner x = not $ S.member (1,1) x
      cs = filter (\x -> isWhite x && isInner x)  $ floodFill px
  results <- mapM (cropToCell i) cs
  return $ map snd $ sortBy (comparing fst) $ zip (map boundingBox cs) results

-- | Run Tesseract on an image containing a single character
tesseract :: Image -> MaybeT IO Int
tesseract img = do
  liftIO $ savePngFile "tesseract.png" img
  h <- liftIO $ runCommand "tesseract -psm 10 tesseract.png tesseract 2> /dev/null"
  liftIO $ waitForProcess h
  r <- liftIO $ readFile "tesseract.txt"
   -- TODO: use maybe properly
  if (null r)
    then return 0
    else return $ (read $ [r!!0])

main :: IO ()
main = do
  i <- loadPngFile imgloc
  rs <- computeCells i
  k <- runMaybeT (mapM tesseract rs)
  let Just l = (transpose . (chunks 9) <$> k)
  mapM_ print l
