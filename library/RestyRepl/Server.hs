{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
module RestyRepl.Server (
        replServer
    ) where

import           Data.Foldable
import           Data.Sequence
import qualified Data.Sequence
import           Data.Text
import qualified Data.Text.Lazy
import           RestyRepl.API                  ( ReplAPI )

import           Servant

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Concurrent.STM.TVar

import           RestyRepl.API


type Line = Data.Text.Lazy.Text

type History = Seq Line

type Input = Text 

-- | What command was sent, and at what line in the REPL's history.
data Interaction = Interaction Input !Int
                 deriving (Eq,Show)

type Interactions = Seq Interaction

addInteraction :: History -> Input -> Interactions -> Interactions
addInteraction history input interactions =  
    let interaction = Interaction input (Data.Sequence.length history)
     in interactions |> interaction 

replServer :: STM History -> (Text -> STM ()) -> IO Application
replServer historyM write = do
    interactionsV <- atomically $ newTVar mempty
    pure $ serve (Proxy @ReplAPI) 
                 (     createInteraction interactionsV historyM write 
                  :<|> readInteraction interactionsV historyM
                  :<|> readHistory historyM)

createInteraction
  :: TVar Interactions
  -> STM History
  -> (Text -> STM ())
  -> Text
  -> Handler InteractionLink
createInteraction interactionsV historyM write input = liftIO $ do
  key <- atomically $ do
    history <- historyM
    key <- Data.Sequence.length <$> readTVar interactionsV
    modifyTVar' interactionsV (addInteraction history input)
    write input
    pure key
  pure
    $ InteractionLink (safeLink (Proxy @ReplAPI) (Proxy @ReadInteraction) key)

readInteraction :: TVar Interactions 
                -> STM History 
                -> Int 
                -> Handler Data.Text.Lazy.Text
readInteraction interactionsV historyM key = liftIO $ do
  atomically $ do
    Just (Interaction  _ start) <- 
        Data.Sequence.lookup key <$> readTVar interactionsV
    history <- historyM
    return $ Data.Text.Lazy.unlines  
           $ toList
           $ Data.Sequence.drop start history 

readHistory :: STM History -> Handler Data.Text.Lazy.Text
readHistory historyM = liftIO $ do
  atomically $ do
    history <- historyM
    pure $ Data.Text.Lazy.unlines (toList history)

