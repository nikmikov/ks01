{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main(main)
where

import System.Environment(getArgs)

import qualified Data.List as L
import Pipes((>->), Producer)
import qualified Pipes.Prelude as P
import Pipes.Safe (runSafeT, SafeT)
import qualified Pipes.ByteString as PB
import qualified Pipes.Safe.Prelude  as PS
import Data.Array.IArray
import System.IO (IOMode(ReadMode))
import Data.Monoid hiding (Product)

import Product

type Solution = Array Int Product

toteLength :: Int
toteLength = 45

toteWidth :: Int
toteWidth  = 30

toteHeight:: Int
toteHeight = 35

toteVolume :: Int
toteVolume = product [toteLength, toteWidth, toteHeight]

isOversize :: CsvProduct -> Bool
isOversize p = csvProductLength p > toteLength 
               || csvProductWidth p > toteWidth
               || csvProductHeight p > toteHeight

printHelp:: IO()
printHelp = putStrLn "Usage:\nks <input file>"


fromFile :: FilePath -> Producer CsvProduct (SafeT IO) ()
fromFile f = csvProducts $ PS.withFile f ReadMode PB.fromHandle 

readData :: FilePath -> (SafeT IO) [Product]
readData f = P.toListM $ 
             fromFile f >-> P.filter ( not . isOversize ) >-> P.map toProduct


addProductToSolution :: Solution -> Product -> Solution
addProductToSolution s p = s  // (map processElem $ assocs s)
    where vol = productVolume p
          val i = mappend (s ! (i - vol) )  p
          processElem (i, !e) = if vol <= i 
                                then (i, max (val i) e)
                                else (i, e)
                                  

solve' :: [Product] -> Solution
solve' = L.foldl addProductToSolution initSolution
    where initSolution = listArray (0, toteVolume) $ repeat mempty

solve :: [Product] -> Product
solve xs = (solve' xs) ! toteVolume

run :: String -> IO()
run p = do
  productList <- runSafeT $ readData p
  let numProducts = length productList
  putStrLn $ "Products: " ++ show (numProducts)
  putStrLn $ "Tote volume: " ++ show (toteVolume)
  let res = solve productList
  putStrLn $ show res
  return()
  

main :: IO()
main = do 
  args <- getArgs
  case args of
    arg:[] -> run arg
    _ -> printHelp
