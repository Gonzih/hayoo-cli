{-# LANGUAGE OverloadedStrings #-}

module HayooTypes
( HayooResult(..)
, HayooResponse(..)
)
where

import Control.Monad (mzero)
import Data.Aeson

data HayooResult = HayooResult { resultUri         :: String
                               , tag               :: String
                               , resultPackage     :: String
                               , resultName        :: String
                               , resultSource      :: String
                               , resultDescription :: String
                               , resultSignature   :: String
                               , resultModules     :: [String]
                               , resultScore       :: Float
                               , resultType        :: String
                               } deriving (Show)

data HayooResponse = HayooResponse { max    :: Int
                                   , offset :: Int
                                   , count  :: Int
                                   , result :: [HayooResult]
                                   } deriving (Show)

instance FromJSON HayooResult where
    parseJSON (Object v) = HayooResult
                           <$> v .:  "resultUri"
                           <*> v .:  "tag"
                           <*> v .:? "resultPackage"     .!= ""
                           <*> v .:  "resultName"
                           <*> v .:? "resultSource"      .!= ""
                           <*> v .:? "resultDescription" .!= ""
                           <*> v .:? "resultSignature"   .!= ""
                           <*> v .:? "resultModules"     .!= []
                           <*> v .:  "resultScore"
                           <*> v .:  "resultType"

    parseJSON _          = mzero

instance FromJSON HayooResponse where
    parseJSON (Object v) = HayooResponse
                           <$> v .: "max"
                           <*> v .: "offset"
                           <*> v .: "count"
                           <*> v .: "result"

    parseJSON _          = mzero
