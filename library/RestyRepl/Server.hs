{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
module RestyRepl.Server (
        replServer
    ) where

import           Data.Foldable
import           Data.Sequence
import qualified Data.Sequence
import           Data.IntMap.Strict (IntMap,insert)
import qualified Data.IntMap.Strict
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

-- | What was sent, and at what point in the REPL's history.
data Interaction = Interaction Input !Int
                 deriving (Eq,Show)

type InteractionId = Int

data Interactions = Interactions !Int !(IntMap Interaction)
                    deriving Show


addInteraction :: History -> Input -> Interactions -> (InteractionId,Interactions)
addInteraction history input (Interactions nextId byId) =  
    let segment = Interaction input (Data.Sequence.length history)
     in (nextId, Interactions (succ nextId) (insert nextId segment byId)) 

replServer :: STM History -> (Text -> STM ()) -> IO Application
replServer historySTM write = do
    ref <- atomically $ newTVar (Interactions 0 mempty)
    pure $ serve (Proxy @ReplAPI) 
                 (     createInteraction ref historySTM write 
                  :<|> readInteraction ref historySTM
                  :<|> readHistory historySTM)

createInteraction
  :: TVar Interactions
  -> STM History
  -> (Text -> STM ())
  -> Text
  -> Handler InteractionLink
createInteraction ref historySTM write input = liftIO $ do
  key <- atomically $ do
    history             <- historySTM
    (key, interactions) <- addInteraction history input <$> readTVar ref
    writeTVar ref interactions
    write input
    pure key
  pure
    $ InteractionLink (safeLink (Proxy @ReplAPI) (Proxy @ReadInteraction) key)

readInteraction :: TVar Interactions 
                -> STM History 
                -> Int 
                -> Handler Data.Text.Lazy.Text
readInteraction ref historySTM key = liftIO $ do
  atomically $ do
    Interactions _ segments <- readTVar $ ref
    history <- historySTM
    let Just (Interaction _ start) = Data.IntMap.Strict.lookup key segments 
    return $ Data.Text.Lazy.unlines  
           $ toList
           $ Data.Sequence.drop start history 

readHistory :: STM History -> Handler Data.Text.Lazy.Text
readHistory historySTM = liftIO $ do
  atomically $ do
    history <- historySTM
    pure $ Data.Text.Lazy.unlines (toList history)

