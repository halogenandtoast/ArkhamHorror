module Arkham.ChaosBag.Base where

import Arkham.Prelude

import Arkham.ChaosBagStepState
import Arkham.ChaosToken
import Arkham.Json

data ChaosBag = ChaosBag
  { chaosBagChaosTokens :: [ChaosToken]
  , chaosBagSetAsideChaosTokens :: [ChaosToken]
  , chaosBagRevealedChaosTokens :: [ChaosToken]
  , chaosBagChoice :: Maybe ChaosBagStepState
  , chaosBagForceDraw :: Maybe ChaosTokenFace
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks)

allChaosBagChaosTokens :: ChaosBag -> [ChaosToken]
allChaosBagChaosTokens ChaosBag {..} =
  chaosBagChaosTokens <> chaosBagSetAsideChaosTokens <> chaosBagRevealedChaosTokens

instance ToJSON ChaosBag where
  toJSON = genericToJSON $ aesonOptions $ Just "chaosBag"
  toEncoding = genericToEncoding $ aesonOptions $ Just "chaosBag"

instance FromJSON ChaosBag where
  parseJSON = genericParseJSON $ aesonOptions $ Just "chaosBag"

emptyChaosBag :: ChaosBag
emptyChaosBag =
  ChaosBag
    { chaosBagChaosTokens = []
    , chaosBagSetAsideChaosTokens = []
    , chaosBagRevealedChaosTokens = []
    , chaosBagChoice = Nothing
    , chaosBagForceDraw = Nothing
    }

chaosTokensL :: Lens' ChaosBag [ChaosToken]
chaosTokensL = lens chaosBagChaosTokens $ \m x -> m {chaosBagChaosTokens = x}

forceDrawL :: Lens' ChaosBag (Maybe ChaosTokenFace)
forceDrawL = lens chaosBagForceDraw $ \m x -> m {chaosBagForceDraw = x}

setAsideChaosTokensL :: Lens' ChaosBag [ChaosToken]
setAsideChaosTokensL =
  lens chaosBagSetAsideChaosTokens $ \m x -> m {chaosBagSetAsideChaosTokens = x}

revealedChaosTokensL :: Lens' ChaosBag [ChaosToken]
revealedChaosTokensL =
  lens chaosBagRevealedChaosTokens $ \m x -> m {chaosBagRevealedChaosTokens = x}

choiceL :: Lens' ChaosBag (Maybe ChaosBagStepState)
choiceL = lens chaosBagChoice $ \m x -> m {chaosBagChoice = x}

createChaosToken :: MonadRandom m => ChaosTokenFace -> m ChaosToken
createChaosToken face = ChaosToken <$> getRandom <*> pure face
