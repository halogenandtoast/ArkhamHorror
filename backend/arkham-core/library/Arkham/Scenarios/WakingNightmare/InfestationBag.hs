module Arkham.Scenarios.WakingNightmare.InfestationBag where

import Arkham.ChaosToken
import Arkham.Prelude
import Arkham.Story.Types
import Data.Aeson (Result (..))
import Data.UUID (nil)
import GHC.Records

newtype InfestationToken = InfestationToken {infestationTokenFace :: ChaosTokenFace}
  deriving newtype (Show, Eq, ToJSON, FromJSON)

asChaosToken :: InfestationToken -> ChaosToken
asChaosToken (InfestationToken face) = ChaosToken (ChaosTokenId nil) face

instance HasField "face" InfestationToken ChaosTokenFace where
  getField = infestationTokenFace

data InfestationBag = InfestationBag
  { infestationTokens :: [InfestationToken]
  , infestationSetAside :: [InfestationToken]
  , infestationCurrentToken :: Maybe InfestationToken
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

initInfestationBag :: InfestationBag
initInfestationBag =
  InfestationBag
    { infestationTokens =
        map InfestationToken [#skull, #tablet, #tablet, #tablet, #tablet, #cultist, #cultist]
    , infestationSetAside = []
    , infestationCurrentToken = Nothing
    }

infestationBag :: StoryAttrs -> InfestationBag
infestationBag attrs = case fromJSON (storyMeta attrs) of
  Success a -> a
  _ -> error "invalid infestation bag"
