{-# LANGUAGE OverloadedStrings #-}

module CliOptions
( parseArguments
, Opts(..)
)
where

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

parseArguments :: (Opts -> IO ()) -> IO ()
parseArguments = (execParser opts >>=)
    where opts = info (helper <*> parser)
            ( fullDesc
           <> progDesc "Search for query using hayoo."
           <> header   "hayoo - 0.1.0.0")
