import Data.List
import Tokens
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noLex

main' = do (fileName : _ ) <- getArgs 
           sourceText <- readFile fileName
           putStrLn ("Lexing : " ++ sourceText)
           let lexedProg = alexScanTokens sourceText
           putStrLn ("Lexed as " ++ (show lexedProg))

noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with Lexing : " ++ err)
             return ()

-------------------------------------------------
factorial :: Int -> Double
factorial 0 = 1.0
factorial 1 = 1.0
factorial n = fromIntegral(n) * factorial (n - 1)

doubfact :: Int -> Double
doubfact 0 = 1.0
doubfact 1 = 1.0
doubfact n = fromIntegral(n) * doubfact (n - 2)

calcPi :: Int -> Int -> [Double] -> [Double]
calcPi n k acc | k>(n-1) = acc
               | otherwise = (form:calcPi n (k+1) acc)
   where form = (factorial k) / (doubfact (2*k+1))

approxPi :: Int -> Double
approxPi n | n<=0 = error "Not valid number"
           | otherwise = 2 * sum (calcPi n 0 [])