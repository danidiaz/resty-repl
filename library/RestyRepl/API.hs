{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module RestyRepl.API (
        ReplAPI,
        InteractionLink(..)
    ) where
import           Servant.API
import           Data.Text
import qualified Data.Text.Lazy
import           Data.Aeson

newtype InteractionLink = InteractionLink { interactionLink :: Text }
                          deriving (Show)

instance ToJSON InteractionLink where
    toJSON (InteractionLink link) =  object ["link" .= link]

type CreateInteraction = 
       ReqBody '[PlainText] Text
    :> Post '[JSON] InteractionLink

type ReadInteraction = 
       Capture "interactionid" Int
    :> Post '[PlainText] Text

type ReadHistory =  
       Get '[PlainText] Text

type ReplAPI = "interaction" :> CreateInteraction
          :<|> "interaction" :> ReadInteraction
          :<|> "history" :> ReadHistory
