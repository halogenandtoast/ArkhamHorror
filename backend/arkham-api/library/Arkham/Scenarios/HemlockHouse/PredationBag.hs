{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Scenarios.HemlockHouse.PredationBag where

import Arkham.ChaosToken
import Arkham.Prelude
import Arkham.Story.Types
import Data.Aeson (Result (..))
import GHC.Records

data PredationToken = PredationToken {predationTokenId :: ChaosTokenId, predationTokenFace :: ChaosTokenFace}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

asChaosToken :: PredationToken -> ChaosToken
asChaosToken (PredationToken tokenId face) = ChaosToken tokenId face Nothing False False

instance HasField "face" PredationToken ChaosTokenFace where
  getField = predationTokenFace

data PredationBag = PredationBag
  { predationTokens :: [PredationToken]
  , predationSetAside :: [PredationToken]
  , predationCurrentToken :: Maybe PredationToken
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "tokens" PredationBag [PredationToken] where
  getField = predationTokens

instance HasField "currentToken" PredationBag (Maybe PredationToken) where
  getField = predationCurrentToken

instance HasField "setAside" PredationBag [PredationToken] where
  getField = predationSetAside

initPredationBag :: MonadRandom m => m PredationBag
initPredationBag = do
  rs <- getRandoms
  pure
    $ PredationBag
      { predationTokens =
          zipWith PredationToken rs [#cultist, #tablet, #elderthing]
      , predationSetAside = []
      , predationCurrentToken = Nothing
      }

predationBag :: StoryAttrs -> PredationBag
predationBag attrs = case fromJSON (storyMeta attrs) of
  Success a -> a
  _ -> error "invalid predation bag"

instance HasField "predationBag" StoryAttrs PredationBag where
  getField = predationBag
