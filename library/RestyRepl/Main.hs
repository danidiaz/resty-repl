module RestyRepl.Main (
           defaultMain
       ) where

import           Options.Applicative
import qualified Options.Applicative as O

import           Control.Concurrent.Async (concurrently_)

import           Network.Wai.Handler.Warp (run,Port)

import           RestyRepl (backgroundRepl)
import           RestyRepl.Server (replServer)


data Options = Options
             {
                port :: Port
             ,  executable :: FilePath
             ,  arguments :: [String]
             } deriving Show

optionsParserInfo :: ParserInfo Options
optionsParserInfo = 
    let mkParserInfo :: Parser a -> String -> ParserInfo a
        mkParserInfo parser desc = 
            O.info (O.helper <*> parser) (O.fullDesc <> O.progDesc desc)
        portOpt = option auto $ short 'p'
                              <> long  "port"
                              <> metavar "PORTNUMBER"
                              <> help "Port on which to open the server"
        programOpt = strOption $ short 'e'
                              <> long  "executable"
                              <> metavar "REPLPATH"
                              <> help "Full path to the REPL's executable"
        argOpt     = strOption $ short 'a'
                              <> long "argument"
                              <> metavar "ARGUMENT"
                              <> help "Arguments to be passed to the executable"
        parser = Options <$> portOpt <*> programOpt <*> many argOpt 
        desc = "Starts a REPL and publishes it as a REST service."
     in O.info (O.helper <*> parser) (O.fullDesc <> O.progDesc desc)

defaultMain :: IO ()
defaultMain = do
    o <- execParser optionsParserInfo
    (historyRef,write,repl) <- backgroundRepl (executable o) (arguments o)
    server <- replServer historyRef write
    concurrently_ repl (run (port o) server)
