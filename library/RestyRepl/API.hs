{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module RestyRepl.API (
        CreateInteraction
    ,   ReadInteraction
    ,   ReadHistory
    ,   ReplAPI
    ,   InteractionLink(..)
    ) where
import           Servant.API
import           Data.Text
import qualified Data.Text.Lazy
import           Data.Aeson

newtype InteractionLink = InteractionLink Link
                          deriving (Show)

instance ToJSON InteractionLink where
    toJSON (InteractionLink link) =  object ["link" .= toUrlPiece link]

type CreateInteraction = 
       "interaction"
    :> ReqBody '[PlainText] Text
    :> Post '[JSON] InteractionLink

type ReadInteraction = 
       "interaction"
    :> Capture "id" Int
    :> Get '[PlainText] Data.Text.Lazy.Text

type ReadHistory =  
       "history"
    :> Get '[PlainText] Data.Text.Lazy.Text

type ReplAPI = CreateInteraction
          :<|> ReadInteraction
          :<|> ReadHistory
