{-# LANGUAGE TemplateHaskell #-}

module Arkham.ChaosBag.Base where

import Arkham.ChaosBagStepState
import Arkham.ChaosToken.Types
import Arkham.Json
import Arkham.Prelude
import Data.Aeson.TH

data ChaosBag = ChaosBag
  { chaosBagChaosTokens :: [ChaosToken]
  , chaosBagSetAsideChaosTokens :: [ChaosToken]
  , chaosBagRevealedChaosTokens :: [ChaosToken]
  , chaosBagChoice :: Maybe ChaosBagStepState
  , chaosBagForceDraw :: Maybe ChaosTokenFace
  , chaosBagTokenPool :: [ChaosToken]
  }
  deriving stock (Show, Eq, Generic)

allChaosBagChaosTokens :: ChaosBag -> [ChaosToken]
allChaosBagChaosTokens ChaosBag {..} =
  chaosBagChaosTokens
    <> chaosBagSetAsideChaosTokens
    <> chaosBagRevealedChaosTokens

$(deriveJSON (aesonOptions $ Just "chaosBag") ''ChaosBag)

emptyChaosBag :: ChaosBag
emptyChaosBag =
  ChaosBag
    { chaosBagChaosTokens = []
    , chaosBagSetAsideChaosTokens = []
    , chaosBagRevealedChaosTokens = []
    , chaosBagChoice = Nothing
    , chaosBagForceDraw = Nothing
    , chaosBagTokenPool = []
    }

chaosTokensL :: Lens' ChaosBag [ChaosToken]
chaosTokensL = lens chaosBagChaosTokens $ \m x -> m {chaosBagChaosTokens = x}

tokenPoolL :: Lens' ChaosBag [ChaosToken]
tokenPoolL = lens chaosBagTokenPool $ \m x -> m {chaosBagTokenPool = x}

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
createChaosToken face = do
  tokenId <- getRandom
  pure $ ChaosToken tokenId face Nothing
