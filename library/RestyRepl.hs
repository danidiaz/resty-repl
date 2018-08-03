{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
module RestyRepl (
        Line
    ,   History
    ,   repl
    ) where

import           Data.Sequence
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Data.Text.Lazy
import           Control.Monad
import           Control.Monad.STM
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TQueue
import           System.IO
import           System.Process.Streaming       ( proc
                                                , piped
                                                , executeInteractive
                                                , foldOut
                                                , withConsumer
                                                , feedProducer
                                                )
import           System.Process.Streaming.Text  ( asUtf8x
                                                , asFoldedLines
                                                )
import           Pipes
import qualified Pipes.Prelude

type Line    = Data.Text.Lazy.Text
type History = Seq Line

-- | Given the full path to an executable and the arguments it should take,
--   return a function to write to stdin, an action that gets
--   history of the REPL session, and an action that actually executes
--   REPL session.
repl :: FilePath 
     -> [String] 
     -> IO (Text -> IO (), TVar History, IO ())
repl executable arguments = do
    inputQueue <- atomically $ newTQueue @Text
    historyRef <- atomically $ newTVar @History mempty
    let execution = 
            executeInteractive 
            (piped (proc executable arguments))
            (feedProducer (producer >-> Pipes.Prelude.map encodeUtf8)
             *> 
             foldOut (asUtf8x (asFoldedLines writeToHistory)))
        producer = 
            forever (do text <- liftIO . atomically $ readTQueue inputQueue
                        yield text)
        writeToHistory = 
            withConsumer $ 
            forever (do line <- await
                        liftIO . atomically $ modifyTVar' historyRef (|> line))
    pure $ (,,) (\text -> atomically (writeTQueue inputQueue text)) 
                historyRef
                execution

