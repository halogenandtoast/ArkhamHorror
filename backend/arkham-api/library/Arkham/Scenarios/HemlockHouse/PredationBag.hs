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
  , predationCancelNext :: Bool
  -- ^ Set by Codex 6 (Gideon Mizrah). The next reveal consumes the flag and
  -- short-circuits without drawing from the bag.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON PredationBag where
  parseJSON = withObject "PredationBag" $ \o -> do
    predationTokens <- o .: "predationTokens"
    predationSetAside <- o .: "predationSetAside"
    predationCurrentToken <- o .:? "predationCurrentToken"
    predationCancelNext <- o .:? "predationCancelNext" .!= False
    pure PredationBag {..}

instance HasField "tokens" PredationBag [PredationToken] where
  getField = predationTokens

instance HasField "currentToken" PredationBag (Maybe PredationToken) where
  getField = predationCurrentToken

instance HasField "setAside" PredationBag [PredationToken] where
  getField = predationSetAside

instance HasField "cancelNext" PredationBag Bool where
  getField = predationCancelNext

initPredationBag :: MonadRandom m => m PredationBag
initPredationBag = do
  rs <- getRandoms
  pure
    $ PredationBag
      { predationTokens =
          zipWith PredationToken rs [#cultist, #tablet, #elderthing]
      , predationSetAside = []
      , predationCurrentToken = Nothing
      , predationCancelNext = False
      }

-- | All tokens currently part of the predation bag, including the in-flight
-- reveal and any set-aside tokens. Used by resolution-2 cleanup to count
-- Tablet tokens before they're returned to the chaos bag.
allBagTokens :: PredationBag -> [PredationToken]
allBagTokens b = predationTokens b <> predationSetAside b <> maybeToList (predationCurrentToken b)

predationBag :: StoryAttrs -> PredationBag
predationBag attrs = case fromJSON (storyMeta attrs) of
  Success a -> a
  _ -> error "invalid predation bag"

instance HasField "predationBag" StoryAttrs PredationBag where
  getField = predationBag
