module RestyRepl.Main (
           defaultMain
       ) where

import           Options.Applicative
import qualified Options.Applicative as O

data Options = Options
             {
                executable :: String
             ,  arguments :: [String]
             } deriving Show

optionsParserInfo :: ParserInfo Options
optionsParserInfo = 
    let mkParserInfo :: Parser a -> String -> ParserInfo a
        mkParserInfo parser desc = 
            O.info (O.helper <*> parser) (O.fullDesc <> O.progDesc desc)
        programOpt = strOption $ short 'e'
                              <> long  "executable"
                              <> metavar "REPLPATH"
                              <> help "Full path to the REPL's executable"
        argOpt     = strOption $ short 'a'
                              <> long "argument"
                              <> metavar "ARGUMENT"
                              <> help "Arguments to be passed to the executable"
        parser = Options <$> programOpt <*> many argOpt
        desc = "Starts a REPL and publishes it as a REST service."
     in O.info (O.helper <*> parser) (O.fullDesc <> O.progDesc desc)

defaultMain :: IO ()
defaultMain = do
    options <- execParser optionsParserInfo
    print options
