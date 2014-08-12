{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Exception (catch)
import System.IO (stderr, hPutStrLn, hPrint)
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson
import Network.HTTP.Conduit
import Text.Pandoc (def, readHtml, writeAsciiDoc)
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..))

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
                           <$> v .: "resultUri"
                           <$> v .: "tag"
                           <*> v .: "resultPackage"
                           <*> v .: "resultName"
                           <*> v .: "resultSource"
                           <*> v .: "resultDescription"
                           <*> v .: "resultSignature"
                           <*> v .: "resultModules"
                           <*> v .: "resultScore"
                           <*> v .: "resultType"

    parseJSON _          = mzero

instance FromJSON HayooResponse where
    parseJSON (Object v) = HayooResponse
                           <$> v .: "max"
                           <*> v .: "offset"
                           <*> v .: "count"
                           <*> v .: "result"

    parseJSON _          = mzero

statusExceptionHandler ::  HttpException -> IO BSL.ByteString
statusExceptionHandler (StatusCodeException status _ _) =
    hPutStrLn stderr "An error occured during download: "
    >> hPrint stderr status
    >> return BSL.empty
statusExceptionHandler exception =
    hPutStrLn stderr "An error occured during download: "
    >> hPrint stderr exception
    >> return BSL.empty

jsonData :: IO BSL.ByteString
jsonData = simpleHttp "http://hayoo.fh-wedel.de/json?query=Monad" `catch` statusExceptionHandler

decodeHayooResponse :: BSL.ByteString -> Maybe HayooResponse
decodeHayooResponse = decode

htmlToAscii :: String -> String
htmlToAscii = (writeAsciiDoc def {writerReferenceLinks = True}) . readHtml def

printResult :: HayooResult -> IO ()
printResult (HayooResult _ _ _ _ _ desc _ _ _ _) = putStrLn $ htmlToAscii desc

printResponse :: HayooResponse -> IO ()
printResponse (HayooResponse _ _ _ results) = mapM_ printResult results

main :: IO ()
main = printResponse =<< decodeHayooResponse =<< jsonData
