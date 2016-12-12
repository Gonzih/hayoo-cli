{-# LANGUAGE OverloadedStrings #-}

module Main where

import CliOptions
import Data.Aeson
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as BSL
import Data.Version (showVersion)
import HayooTypes
import Network.HTTP.Conduit
import Network.HTTP.Types.Header (hAccept, hUserAgent)
import Network.URL (encString, ok_url)
import Paths_hayoo_cli (version)
import Text.Pandoc (def, readHtml, writeAsciiDoc)
import Text.Pandoc.Options (WriterOptions (..))

jsonData :: Opts -> IO BSL.ByteString
jsonData (Opts _ searchQuery) = do
    req <- parseUrlThrow url
    let req' = req { requestHeaders = [acceptJSONHeader, userAgentHeader] }
    manager <- newManager tlsManagerSettings
    response <- httpLbs req' manager
    return $ responseBody response
    where url              = "http://hayoo.fh-wedel.de/json?query=" ++ encQuery
          encQuery         = encString True ok_url searchQuery
          userAgent        = pack $ "hayoo-cli/" ++ showVersion version
          userAgentHeader  = (hUserAgent, userAgent)
          acceptJSONHeader = (hAccept, "application/json")

decodeHayooResponse :: BSL.ByteString -> HayooResponse
decodeHayooResponse bs = either (error . show) id (eitherDecode bs)

htmlToAscii :: String -> String
htmlToAscii = writeAsciiDoc def{writerReferenceLinks = True} . either (error . show) id . readHtml def

printResultFull :: HayooResult -> IO ()
printResultFull singleResult@HayooResult{ resultDescription = desc } =
    printResultShort singleResult
    >> putStrLn ""
    >> putStrLn (htmlToAscii desc)

printResultShort :: HayooResult -> IO ()
printResultShort (HayooResult { resultName      = name
                              , resultSignature = signature
                              , resultModules   = modules
                              , resultPackage   = package
                              }) =
    putStrLn $ unwords $ package : modules ++ nameAndSignature name signature
    where nameAndSignature n "" = [n]
          nameAndSignature n s  = [n, "::", s]

printResponse :: Opts -> HayooResponse -> IO ()
printResponse (Opts _ _)     HayooResponse{ result = [] }      = putStrLn "No results found"
printResponse (Opts True _)  HayooResponse{ result = (res:_) } = printResultFull res
printResponse (Opts False _) HayooResponse{ result = results } = mapM_ printResultShort results

run :: Opts -> IO ()
run opts =
    jsonData opts
    >>= (printResponse opts . decodeHayooResponse)

main :: IO ()
main = parseArguments ver run
       where ver = showVersion version
