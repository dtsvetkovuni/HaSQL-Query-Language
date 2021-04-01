import Control.Exception
import System.IO
import Data.List

fileMZip :: () -> IO ()
fileMZip () = catch ( fileMZip' () ) filehandler

filehandler :: IOException -> IO ()
filehandler e = do let errMsg = show (e :: IOException)
                   hPutStr stderr ("Warning - File not found : " ++ errMsg)
                   return ()

multiZipL :: [[a]] -> [[a]]
multiZipL [] = []
multiZipL ([] : xss) = multiZipL xss
multiZipL ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : multiZipL (xs : [ t | (_:t) <- xss])

fileMZip' :: () -> IO ()
fileMZip' () = do
  s <- readFile "test.csv"
  let ls = lines s
  let sss = map (splitOn ',') ls
--  let iss = map (map read) sss :: [[Int]]
  let mss = multiZipL sss
--  let oss = map (map show) mss
  let css = intercalate "\n" (map (intercalate ",") mss)
  writeFile "testout.csv" css
  
splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c ls = takeWhile (/=c) ls : splitOn' c (dropWhile (/=c) ls)
 where splitOn' c [] = []
       splitOn' c [x] | x==c = [[]]
       splitOn' c (x:xs) | x==c = splitOn c xs
                         | otherwise = []