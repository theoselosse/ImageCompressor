import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Data.String()
import Data.List (elemIndex)
import Data.Char (isDigit)
import Data.Typeable()
import Text.Read (readMaybe)
import Numeric()
import Control.Exception()
import System.Directory (doesFileExist)
import System.Random (newStdGen, Random(randomR), StdGen)
import Data.Maybe (mapMaybe)
import Text.Printf

---data declarations

data Point = Point Int Int

data Color = Color Int Int Int

data Pixel = Pixel Point Color

---program beginning

main :: IO ()
main = do
    args <- getArgs
    let r_length = myLength args
    let b_length = checkLength r_length
    if b_length then (do
        let r_args = tupleArgs args
        let b_args = checkArgs r_args
        if b_args then parseArgs r_args else usage >> failure) else usage >> failure

---number, limit and path arguments parsing function

parseArgs :: [(String, String)] -> IO ()
parseArgs r_args = do
        let s_number = getNumber r_args
        let b_ndigit = isANumber s_number
        if b_ndigit then (do
            let number = read s_number :: Int
            let b_number = checkNumber number
            if b_number then (do
                let s_limit = getLimit r_args
                let b_ldigit = isANumber s_limit
                if b_ldigit then (do
                    let limit = read s_limit :: Double
                    let b_limit = checkLimit limit
                    if b_limit then (do
                        let path = getPath r_args
                        x <- doesFileExist path
                        if x then getClustering number limit path else usage >> failure) else usage >> failure) else usage >> failure) else usage >> failure) else usage >> failure

---check if values are correct

limitClusters :: [Pixel] -> [Pixel] -> Double -> Int -> Bool
limitClusters cluster new_cluster limit it
    | it >= length cluster - 1 = True
    | checkClusters (cluster!!(it + 1)) (new_cluster!!(it + 1)) > limit = False
    | checkClusters (cluster!!(it + 1)) (new_cluster!!(it + 1)) == limit = False
    | checkClusters (cluster!!(it + 1)) (new_cluster!!(it + 1)) < limit = limitClusters cluster new_cluster limit (it + 1)

checkClusters :: Pixel -> Pixel -> Double
checkClusters (Pixel (Point _ _) (Color ro go bo)) (Pixel (Point _ _) (Color rp gp bp)) = getPointDistance (fromIntegral ro) (fromIntegral rp) (fromIntegral go) (fromIntegral gp) (fromIntegral bo) (fromIntegral bp)

checkLength :: Int -> Bool
checkLength x = x == 6

checkArgs :: [(String, String)] -> Bool
checkArgs ((x,_):xs)
    | x == "-n" || x == "-l" || x == "-f" = checkArgs xs
    | otherwise = False
checkArgs [] = True

checkNumber :: Int -> Bool
checkNumber x = x > 1

checkLimit :: Double -> Bool
checkLimit x = x > 0

isANumber :: String -> Bool
isANumber ""  = False
isANumber "." = False
isANumber xs  =
  case dropWhile isDigit xs of
    "" -> True
    ('.':ys) -> all isDigit ys
    _ -> False

---exit functions

usage :: IO ()
usage = putStrLn "Usage: ./imageCompressor -n \"number\" -l \"limit\" -f \"path\""

failure :: IO a
failure = exitWith (ExitFailure 84)

---get the infos

myLength :: [String] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

getNumber :: [(String, String)] -> String
getNumber ((x,y):xs)
    | x == "-n" = y
    | otherwise = getNumber xs
getNumber [] = "empty"

getLimit :: [(String, String)] -> String
getLimit ((x,y):xs)
    | x == "-l" = y
    | otherwise = getLimit xs
getLimit [] = "empty"

getPath :: [(String, String)] -> String
getPath ((x,y):xs)
    | x == "-f" = y
    | otherwise = getPath xs
getPath [] = "empty"

---convert values in one

tupleArgs :: [String] -> [(String, String)]
tupleArgs [] = []
tupleArgs [_] = []
tupleArgs (x:y:xs) = (x, y) : tupleArgs xs

---get file infos

getClustering :: Int -> Double -> String -> IO ()
getClustering nbr limit path = do
    file <- readFile path
    g <- newStdGen
    let pixel = getFileLine file
    let cluster = createCluster pixel [] g nbr
    let index = sortClusters pixel cluster (-1) []
    let new_cluster = redefineClusterLoop cluster index pixel limit
    let new_index = redefineIndexLoop cluster index pixel limit
    printResults new_cluster new_index pixel 0

getFileLine ::String -> [Pixel]
getFileLine file = mapMaybe readFileLine (lines file)

readFileLine :: String -> Maybe Pixel
readFileLine line = let index = elemIndex ')' line
                    in case index of
                        Just index_o -> tupleToPixel first second
                            where tmp = splitAt (index_o + 1) line
                                  first = readMaybe $ fst tmp :: Maybe (Int, Int)
                                  second = readMaybe $ snd tmp :: Maybe (Int, Int, Int)
                        Nothing -> Nothing

tupleToPixel :: Maybe (Int, Int) -> Maybe (Int, Int, Int) -> Maybe Pixel
tupleToPixel Nothing _ = Nothing
tupleToPixel _ Nothing = Nothing
tupleToPixel (Just (x, y)) (Just (r, g, b))
    | x < 0 || y < 0 = Nothing
    | r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 = Nothing
    | otherwise = Just $ Pixel (Point x y) (Color r g b)

---clustering

createCluster :: [Pixel] -> [Pixel] -> StdGen -> Int -> [Pixel]
createCluster [] _ _ _ = []
createCluster pixel cluster rand nbr
    | nbr <= 0 = cluster
    | otherwise = createCluster pixel (pixel!!(random):cluster) new_rand (nbr - 1)
    where (random, new_rand) = randomR (0, length pixel - 1) rand

sortClusters :: [Pixel] -> [Pixel] -> Int -> [Int] -> [Int]
sortClusters [] [] _ _ = []
sortClusters pixel cluster lgt tab
    | lgt >= length pixel - 1 = tab
    | otherwise = sortClusters pixel cluster (lgt + 1) ((getNearestClust (pixel!!(lgt + 1)) cluster (-1) (-1) (-1)):tab)

getNearestClust :: Pixel -> [Pixel] -> Double -> Int -> Int -> Int
getNearestClust _ [] _ _ _ = (-1)
getNearestClust pixel cluster var_min it index_min
    | it >= length cluster - 1 = index_min
    | otherwise = getNearestClust pixel cluster (checkAllClustVarMin var_min pixel (cluster!!(it + 1))) (it + 1) (checkAllClustIndexMin var_min pixel (cluster!!(it + 1)) (it + 1) index_min)

checkAllClustVarMin :: Double -> Pixel -> Pixel -> Double
checkAllClustVarMin var_min (Pixel (Point _ _) (Color ra ga ba)) (Pixel (Point _ _) (Color rb gb bb))
    | var_min < 0 = getPointDistance (fromIntegral rb) (fromIntegral ra) (fromIntegral gb) (fromIntegral ga) (fromIntegral bb) (fromIntegral ba)
    | getPointDistance (fromIntegral rb) (fromIntegral ra) (fromIntegral gb) (fromIntegral ga) (fromIntegral bb) (fromIntegral ba) > var_min = getPointDistance (fromIntegral rb) (fromIntegral ra) (fromIntegral gb) (fromIntegral ga) (fromIntegral bb) (fromIntegral ba)
    | var_min >= getPointDistance (fromIntegral rb) (fromIntegral ra) (fromIntegral gb) (fromIntegral ga) (fromIntegral bb) (fromIntegral ba) = var_min

checkAllClustIndexMin :: Double -> Pixel -> Pixel -> Int -> Int -> Int
checkAllClustIndexMin var_min (Pixel (Point _ _) (Color ra ga ba)) (Pixel (Point _ _) (Color rb gb bb)) it index_min
    | var_min < 0 = it
    | getPointDistance (fromIntegral rb) (fromIntegral ra) (fromIntegral gb) (fromIntegral ga) (fromIntegral bb) (fromIntegral ba) > var_min = it
    | var_min >= getPointDistance (fromIntegral rb) (fromIntegral ra) (fromIntegral gb) (fromIntegral ga) (fromIntegral bb) (fromIntegral ba) = index_min

---distance calculation 

getPointDistance :: Double -> Double -> Double -> Double -> Double -> Double -> Double
getPointDistance xa xb ya yb za zb = sqrt((xb-xa)^2 + (yb-ya)^2 + (zb-za)^2)

---clusters adjusting

redefineClusterLoop :: [Pixel] -> [Int] -> [Pixel] -> Double -> [Pixel]
redefineClusterLoop cluster index pixel limit = do
    let new_cluster = adjustCluster pixel cluster index [] limit (-1)
    let b_clusters = limitClusters cluster new_cluster limit (-1)
    if b_clusters then redefineClusterLoop new_cluster (sortClusters pixel new_cluster (-1) []) pixel (limit - 1) else new_cluster

redefineIndexLoop :: [Pixel] -> [Int] -> [Pixel] -> Double -> [Int]
redefineIndexLoop cluster index pixel limit = do
    let new_cluster = adjustCluster pixel cluster index [] limit (-1)
    let b_clusters = limitClusters cluster new_cluster limit (-1)
    if b_clusters then redefineIndexLoop new_cluster (sortClusters pixel new_cluster (-1) []) pixel (limit - 1) else index

adjustCluster :: [Pixel] -> [Pixel] -> [Int] -> [Pixel] -> Double -> Int -> [Pixel]
adjustCluster pixel cluster index new_cluster limit it
    | it >= length cluster - 1 = new_cluster
    | otherwise = adjustCluster pixel cluster index (((clusterAverage pixel index (it + 1) (-1) (Pixel (Point (0) (0)) (Color (0) (0) (0))) 0 (cluster!!(it + 1)))):new_cluster) limit (it + 1)

clusterAverage :: [Pixel] -> [Int] -> Int -> Int -> Pixel -> Int -> Pixel -> Pixel
clusterAverage pixel index it it_index (Pixel (Point x y) (Color r g b)) nbr old_pixel
    | it_index < 0 = clusterAverage pixel index it (it_index + 1) (Pixel (Point x y) (Color r g b)) nbr old_pixel
    | index!!it_index == it && it_index < length index - 1 = clusterAverage pixel index it (it_index + 1) (addPixels (pixel!!it_index) (Pixel (Point x y) (Color r g b))) (nbr + 1) old_pixel
    | index!!it_index /= it && it_index < length index - 1 = clusterAverage pixel index it (it_index + 1) (Pixel (Point x y) (Color r g b)) nbr old_pixel
    | otherwise = (pixelAverage (Pixel (Point x y) (Color r g b)) nbr old_pixel)

addPixels :: Pixel -> Pixel -> Pixel
addPixels (Pixel (Point a b) (Color r1 g1 b1)) (Pixel (Point _ _) (Color r2 g2 b2)) = (Pixel (Point a b) (Color (r1 + r2) (g1 + g2) (b1 + b2)))

pixelAverage :: Pixel -> Int -> Pixel -> Pixel
pixelAverage (Pixel (Point x y) (Color r g b)) nbr old_pixel
    | nbr == 0 = old_pixel
    | otherwise = (Pixel (Point x y) (Color (r `div` nbr) (g `div` nbr) (b `div` nbr)))

---print results

printResults :: [Pixel] -> [Int] -> [Pixel] -> Int -> IO ()
printResults [] _ _ _ = return ()
printResults (x:xs) index pixel it  = do printCluster x index pixel it >> printResults xs index pixel (it + 1)

printCluster :: Pixel -> [Int] -> [Pixel] -> Int -> IO ()
printCluster (Pixel (Point _ _) (Color r g b)) index pixel it = do
            printf "--\n(%d,%d,%d)\n-\n" r g b
            printPixel index pixel it 0

printPixel :: [Int] -> [Pixel] -> Int -> Int -> IO ()
printPixel index pixel it it_index
    | it_index > length index - 1 = printf ""
    | index!!it_index == it && it_index <= length index - 1 = printOnePixel (pixel!!it_index) >> printPixel index pixel it (it_index + 1)
    | index!!it_index /= it && it_index <= length index - 1 = printPixel index pixel it (it_index + 1)

printOnePixel :: Pixel -> IO()
printOnePixel (Pixel (Point x y) (Color r g b)) = do
            printf "(%d,%d) (%d,%d,%d)\n" x y r g b