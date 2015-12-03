{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Product(CsvProduct(..), Product(..),  csvProducts, toProduct)
where

import Control.Monad
import Control.Applicative
import Data.Monoid hiding (Product)
import Pipes.Csv as Csv
import Pipes.Safe(MonadSafe)
import Pipes (Pipe, Producer, await, yield, (>->))
import qualified Data.ByteString as BS
import qualified Data.Vector as V

import Prelude hiding(length)

data CsvProduct = CsvProduct {
      csvProductId :: Int
    , csvProductPrice :: Int
    , csvProductLength :: Int
    , csvProductWidth :: Int
    , csvProductHeight :: Int
    , csvProductWeight :: Int
} deriving (Show)

data Product = Product {
      productId :: !Int
    , productPrice :: !Int
    , productVolume :: !Int
    , productWeight :: !Int
    , productNum :: !Int
} deriving (Show, Eq)

instance Monoid Product where
    mempty = Product 0 0 0 0 0
    mappend x y = Product (productId x + productId y) 
                          (productPrice x + productPrice y)
                          (productVolume x + productVolume y)
                          (productWeight x + productWeight y)
                          (productNum x + productNum y)

instance Ord Product where
    compare = compareProducts

instance FromRecord CsvProduct where
    parseRecord v
        | V.length v == 6 =  CsvProduct <$> 
                           v .! 0 <*> v .! 1 <*> v .! 2 <*> 
                           v .! 3 <*> v .! 4 <*> v .! 5
        | otherwise     = mzero

compareProducts :: Product -> Product -> Ordering
compareProducts lhs rhs = if cmpPriceAsc == EQ
                            then cmpWeightDesc
                            else cmpPriceAsc
    where cmpPriceAsc = compare (productPrice lhs) (productPrice rhs)
          cmpWeightDesc = compare (productWeight rhs) (productWeight lhs)

toProduct :: CsvProduct -> Product
toProduct p = Product (csvProductId p) (csvProductPrice p)
              (product [csvProductHeight p, csvProductWidth p, csvProductLength p])
              (csvProductWeight p) 1

csvProducts :: MonadSafe m 
               => Producer BS.ByteString m () -> Producer CsvProduct m ()
csvProducts p = Csv.decode NoHeader p  >-> right

right :: MonadSafe m => Pipe (Either a b) b m r
right = loop
    where
      loop = await >>= \s -> case s of
                               Left _  -> loop
                               Right v -> yield v >> loop

