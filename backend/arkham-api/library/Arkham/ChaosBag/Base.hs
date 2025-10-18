{-# LANGUAGE TemplateHaskell #-}

module Arkham.ChaosBag.Base where

import Arkham.ChaosBagStepState
import Arkham.ChaosToken.Types
import Arkham.Json
import Arkham.Prelude
import Data.Aeson.TH
import GHC.Records

data ChaosBag = ChaosBag
  { chaosBagChaosTokens :: [ChaosToken]
  , chaosBagSetAsideChaosTokens :: [ChaosToken]
  , chaosBagRevealedChaosTokens :: [ChaosToken]
  , chaosBagChoice :: Maybe ChaosBagStepState
  , chaosBagForceDraw :: Maybe ChaosTokenFace
  , chaosBagTokenPool :: [ChaosToken]
  , chaosBagTotalRevealedChaosTokens :: [ChaosToken]
  }
  deriving stock (Show, Eq, Generic)

allChaosBagChaosTokens :: ChaosBag -> [ChaosToken]
allChaosBagChaosTokens ChaosBag {..} =
  nub
    $ chaosBagChaosTokens
    <> chaosBagSetAsideChaosTokens
    <> chaosBagRevealedChaosTokens

$(deriveToJSON (aesonOptions $ Just "chaosBag") ''ChaosBag)

instance FromJSON ChaosBag where
  parseJSON = withObject "ChaosBag" \o -> do
    chaosBagChaosTokens <- o .: "chaosTokens"
    chaosBagSetAsideChaosTokens <- o .: "setAsideChaosTokens"
    chaosBagRevealedChaosTokens <- o .: "revealedChaosTokens"
    chaosBagChoice <- o .:? "choice"
    chaosBagForceDraw <- o .:? "forceDraw"
    chaosBagTokenPool <- o .: "tokenPool"
    chaosBagTotalRevealedChaosTokens <- o .:? "totalRevealedChaosTokens" .!= []
    pure ChaosBag {..}

emptyChaosBag :: ChaosBag
emptyChaosBag =
  ChaosBag
    { chaosBagChaosTokens = []
    , chaosBagSetAsideChaosTokens = []
    , chaosBagRevealedChaosTokens = []
    , chaosBagChoice = Nothing
    , chaosBagForceDraw = Nothing
    , chaosBagTokenPool = []
    , chaosBagTotalRevealedChaosTokens = []
    }

instance HasField "revealed" ChaosBag [ChaosToken] where
  getField = chaosBagRevealedChaosTokens

instance HasField "totalRevealed" ChaosBag [ChaosToken] where
  getField = chaosBagTotalRevealedChaosTokens

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

totalRevealedChaosTokensL :: Lens' ChaosBag [ChaosToken]
totalRevealedChaosTokensL =
  lens chaosBagTotalRevealedChaosTokens $ \m x -> m {chaosBagTotalRevealedChaosTokens = x}

choiceL :: Lens' ChaosBag (Maybe ChaosBagStepState)
choiceL = lens chaosBagChoice $ \m x -> m {chaosBagChoice = x}
