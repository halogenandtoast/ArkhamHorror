{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Scenarios.WakingNightmare.InfestationBag where

import Arkham.ChaosToken
import Arkham.Prelude
import Arkham.Story.Types
import Data.Aeson (Result (..))
import GHC.Records

data InfestationToken = InfestationToken {infestationTokenId :: ChaosTokenId, infestationTokenFace :: ChaosTokenFace}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

asChaosToken :: InfestationToken -> ChaosToken
asChaosToken (InfestationToken tokenId face) = ChaosToken tokenId face Nothing False False

instance HasField "face" InfestationToken ChaosTokenFace where
  getField = infestationTokenFace

data InfestationBag = InfestationBag
  { infestationTokens :: [InfestationToken]
  , infestationSetAside :: [InfestationToken]
  , infestationCurrentToken :: Maybe InfestationToken
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "tokens" InfestationBag [InfestationToken] where
  getField = infestationTokens

instance HasField "currentToken" InfestationBag (Maybe InfestationToken) where
  getField = infestationCurrentToken

instance HasField "setAside" InfestationBag [InfestationToken] where
  getField = infestationSetAside

initInfestationBag :: MonadRandom m => m InfestationBag
initInfestationBag = do
  rs <- getRandoms
  pure
    $ InfestationBag
      { infestationTokens =
          zipWith InfestationToken rs [#skull, #tablet, #tablet, #tablet, #tablet, #cultist, #cultist]
      , infestationSetAside = []
      , infestationCurrentToken = Nothing
      }

infestationBag :: StoryAttrs -> InfestationBag
infestationBag attrs = case fromJSON (storyMeta attrs) of
  Success a -> a
  _ -> error "invalid infestation bag"

instance HasField "infestationBag" StoryAttrs InfestationBag where
  getField = infestationBag
