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

type Input = Text 

type Line = Data.Text.Lazy.Text

type History = Seq Line

data Segment = Segment !Int
               deriving (Eq,Show)

type InteractionId = Int

data Interactions = Interactions !Int !(IntMap (Input, Segment))
                    deriving Show


addInteraction :: History -> Input -> Interactions -> (InteractionId,Interactions)
addInteraction history input (Interactions nextId byId) =  
    let segment = Segment (Data.Sequence.length history)
     in (nextId, Interactions (succ nextId) (insert nextId (input,segment) byId)) 

replServer :: TVar History -> (Text -> STM ()) -> IO Application
replServer historyRef write = do
    ref <- atomically $ newTVar (Interactions 0 mempty)
    pure $ serve (Proxy @ReplAPI) 
                 (     createInteraction ref historyRef write 
                  :<|> readInteraction ref historyRef
                  :<|> readHistory historyRef)

createInteraction
  :: TVar Interactions
  -> TVar History
  -> (Text -> STM ())
  -> Text
  -> Handler InteractionLink
createInteraction ref historyRef write input = liftIO $ do
  key <- atomically $ do
    history             <- readTVar $ historyRef
    (key, interactions) <- addInteraction history input <$> readTVar ref
    writeTVar ref interactions
    write input
    pure key
  pure
    $ InteractionLink (safeLink (Proxy @ReplAPI) (Proxy @ReadInteraction) key)

readInteraction :: TVar Interactions 
                -> TVar History 
                -> Int 
                -> Handler Data.Text.Lazy.Text
readInteraction ref historyRef key = liftIO $ do
  text <- atomically $ do
        Interactions _ segments <- readTVar $ ref
        history             <- readTVar $ historyRef
        let Just (_,Segment start) = Data.IntMap.Strict.lookup key segments 
        return $ Data.Text.Lazy.unlines  
               $ toList
               $ Data.Sequence.drop start history 
  pure text

readHistory :: TVar History -> Handler Data.Text.Lazy.Text
readHistory historyRef = liftIO $ do
  fulltext <- atomically $ do
    history             <- readTVar $ historyRef
    pure $ Data.Text.Lazy.unlines (toList history)
  pure fulltext

