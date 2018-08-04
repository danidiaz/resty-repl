{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
module RestyRepl.Server (
        replServer
    ) where

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

type Line    = Data.Text.Lazy.Text
type History = Seq Line
type Input   = Text 

data Segment = Segment 
             { 
               start :: Int
             , end :: Maybe Int 
             } deriving (Eq,Show)

data Interactions = Interactions 
                  { 
                        nextInteractionId :: !Int
                  ,     interactionMap :: !(IntMap (Input, Segment))
                  } deriving Show

replServer :: (Text -> STM ()) -> TVar History -> IO Application
replServer write historyRef = do
    ref <- atomically $ newTVar (Interactions 0 mempty)
    pure $ serve (Proxy @ReplAPI) 
                 (     createInteraction ref write historyRef 
                  :<|> readInteraction ref historyRef
                  :<|> readHistory historyRef)

createInteraction :: TVar Interactions
                  -> (Text -> STM ()) 
                  -> TVar History 
                  -> Text 
                  -> Handler InteractionLink
createInteraction ref write historyRef input = liftIO $ do
    interactionId <- atomically $ do
        interactions <- readTVar $ ref
        history <- readTVar $ historyRef
        let len = Data.Sequence.length history
            interactionId = nextInteractionId interactions
            interactions' = 
                interactions { nextInteractionId = succ interactionId 
                             , interactionMap = insert interactionId 
                                                       (input,Segment len Nothing)
                                                       (interactionMap interactions)
                             }
        write input
        undefined -- modifyTVar' 
    undefined

readInteraction :: TVar Interactions 
                -> TVar History 
                -> Int 
                -> Handler Text
readInteraction ref historyRef interactionId = undefined

readHistory :: TVar History -> Handler Text
readHistory historyRef = undefined
