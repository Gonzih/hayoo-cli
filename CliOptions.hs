{-# LANGUAGE OverloadedStrings #-}

module CliOptions
( parseArguments
, Opts(..)
)
where

import Data.Monoid
import Options.Applicative

data Opts = Opts
    { showInfo :: Bool
    , query    :: String }
    deriving Show

parser :: Parser Opts
parser = Opts
    <$> switch
        ( long "info"
       <> short 'i'
       <> help "Show description")
    <*> argument str (metavar "QUERY")

parseArguments :: String -> (Opts -> IO ()) -> IO ()
parseArguments ver = (execParser opts >>=)
    where opts = info (helper <*> parser)
            ( fullDesc
           <> progDesc "Search for query using hayoo."
           <> header   ("hayoo - " ++ ver))
