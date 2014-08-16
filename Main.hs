{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch)
import System.IO (stderr, hPutStrLn, hPrint)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import Data.Aeson
import Network.HTTP.Conduit
import Text.Pandoc (def, readHtml, writeAsciiDoc)
import Text.Pandoc.Options (WriterOptions(..))
import CliOptions
import HayooTypes

statusExceptionHandler ::  HttpException -> IO BSL.ByteString
statusExceptionHandler (StatusCodeException status _ _) =
    hPutStrLn stderr "An error occured during download: "
    >> hPrint stderr status
    >> return BSL.empty
statusExceptionHandler exception =
    hPutStrLn stderr "An error occured during download: "
    >> hPrint stderr exception
    >> return BSL.empty

jsonData :: Opts -> IO BSL.ByteString
jsonData (Opts _ sQuery) = simpleHttp url `catch` statusExceptionHandler
                           where url = "http://hayoo.fh-wedel.de/json?query=" ++ sQuery

decodeHayooResponse :: BSL.ByteString -> Maybe HayooResponse
decodeHayooResponse = decode

printDelimiter :: Char -> IO ()
printDelimiter = putStrLn . replicate 100

htmlToAscii :: String -> String
htmlToAscii = (writeAsciiDoc def {writerReferenceLinks = True}) . readHtml def

printResultFull :: HayooResult -> IO ()
printResultFull singleResult@(HayooResult _ _ _ _ _ desc _ _ _ _) =
    putStrLn ""
    >> printResultShort singleResult
    >> printDelimiter '-'
    >> (putStrLn $ htmlToAscii desc)
    >> printDelimiter '='

printResultShort :: HayooResult -> IO ()
printResultShort (HayooResult _ _ _ _ _ _ signature modules _ _) =
    putStrLn $ unwords $ modules ++ [signature]

printResponse :: Opts -> HayooResponse -> IO ()
printResponse (Opts True _)  (HayooResponse _ _ _ results) = mapM_ printResultFull results
printResponse (Opts False _) (HayooResponse _ _ _ results) = mapM_ printResultShort results

run :: Opts -> IO ()
run opts =
    (jsonData opts)
    >>= (return . decodeHayooResponse)
    >>= (F.mapM_ $ printResponse opts)

main :: IO ()
main = parseArguments run
