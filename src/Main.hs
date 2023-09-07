{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad (forM_)
import           System.IO     (hPutStrLn, putStrLn, stderr)
import           Text.Printf   (printf)
import           Utils         (Vec3 (..), colorSpace, printPixel)

main :: IO ()
main = do
    --args <- getArgs
    --case args of
    --  [] -> getContents >>= run
    --  fs -> foldM mergeFile "" (reverse fs) >>= run
    putStrLn "P3"
    let imageWidth :: Integer = 256
    let imageHeight :: Integer = 256
    putStrLn $ printf "%d %d" imageWidth imageHeight
    putStrLn $ show colorSpace
    forM_ [1..imageHeight] (\i -> do
        hPutStrLn stderr $ printf "Remaining lines: %d" (imageHeight - i + 1)
        forM_ [1..imageWidth] (\j -> do
            let color = Vec3 { x = fromInteger j / (fromInteger $ imageWidth - 1), y = fromInteger i / (fromInteger $ imageHeight - 1), z = 0 }
            printPixel color))
    hPutStrLn stderr "Done!"
