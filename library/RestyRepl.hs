{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
module RestyRepl (
        repl
    ) where

import           Data.Sequence
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy
import           Control.Monad
import           Control.Monad.STM
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TQueue
import           System.IO
import           System.Process.Streaming ( proc
                                          , piped
                                          , executeInteractive
                                          , foldOut 
                                          , withConsumer 
                                          , feedProducer)
import           System.Process.Streaming.Text ( asUtf8x
                                               , asFoldedLines
                                               )
import           Pipes
import qualified Pipes.Prelude

type History = Seq Data.Text.Lazy.Text

repl :: FilePath -> [String] -> IO (Text -> IO (), IO History, IO ())
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
                (atomically $ readTVar historyRef)
                execution

