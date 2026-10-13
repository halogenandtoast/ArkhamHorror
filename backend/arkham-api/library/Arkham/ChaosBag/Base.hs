{-# LANGUAGE TemplateHaskell #-}

module Arkham.ChaosBag.Base where

import Arkham.ChaosBagStepState
import Arkham.ChaosToken.Types
import Arkham.Json
import Arkham.Prelude
import Arkham.Source
import Data.Aeson.TH
import GHC.Records

{- | A token drawn by the debug sandbox reveal: out of the bag, but never
revealed to the game. The value is a snapshot of the scenario reference card
(and elder sign) at draw time, for the debug calculator only. 'Nothing' with
neither auto flag set means the engine could not work it out outside a skill
test, and the player fills it in.
-}
data DebugRevealedChaosToken = DebugRevealedChaosToken
  { debugRevealedToken :: ChaosToken
  , debugRevealedValue :: Maybe Int
  , debugRevealedAutoFail :: Bool
  , debugRevealedAutoSuccess :: Bool
  }
  deriving stock (Show, Eq, Generic)

data ChaosBag = ChaosBag
  { chaosBagChaosTokens :: [ChaosToken]
  , chaosBagSetAsideChaosTokens :: [ChaosToken]
  , chaosBagRevealedChaosTokens :: [ChaosToken]
  , chaosBagChoice :: Maybe ChaosBagStepState
  , chaosBagForceDraw :: [ChaosTokenFace]
  , chaosBagTokenPool :: [ChaosToken]
  , chaosBagTotalRevealedChaosTokens :: [ChaosToken]
  , chaosBagPendingRequests :: Map Source [ChaosToken]
  , chaosBagDebugRevealedChaosTokens :: [DebugRevealedChaosToken]
  }
  deriving stock (Show, Eq, Generic)

-- Sandbox tokens are out of the bag only for the debug reveal; they still
-- belong to it, so they count here.
allChaosBagChaosTokens :: ChaosBag -> [ChaosToken]
allChaosBagChaosTokens ChaosBag {..} =
  nub
    $ chaosBagChaosTokens
    <> chaosBagSetAsideChaosTokens
    <> chaosBagRevealedChaosTokens
    <> map debugRevealedToken chaosBagDebugRevealedChaosTokens

mconcat
  [ deriveJSON (aesonOptions $ Just "debugRevealed") ''DebugRevealedChaosToken
  , deriveToJSON (aesonOptions $ Just "chaosBag") ''ChaosBag
  ]

instance FromJSON ChaosBag where
  parseJSON = withObject "ChaosBag" \o -> do
    chaosBagChaosTokens <- o .: "chaosTokens"
    chaosBagSetAsideChaosTokens <- o .: "setAsideChaosTokens"
    chaosBagRevealedChaosTokens <- o .: "revealedChaosTokens"
    chaosBagChoice <- o .:? "choice"
    -- old exports stored a single face
    chaosBagForceDraw <- (o .:? "forceDraw" .!= []) <|> (maybeToList <$> (o .:? "forceDraw"))
    chaosBagTokenPool <- o .: "tokenPool"
    chaosBagTotalRevealedChaosTokens <- o .:? "totalRevealedChaosTokens" .!= []
    chaosBagPendingRequests <- o .:? "pendingRequests" .!= mempty
    chaosBagDebugRevealedChaosTokens <- o .:? "debugRevealedChaosTokens" .!= []
    pure ChaosBag {..}

emptyChaosBag :: ChaosBag
emptyChaosBag =
  ChaosBag
    { chaosBagChaosTokens = []
    , chaosBagSetAsideChaosTokens = []
    , chaosBagRevealedChaosTokens = []
    , chaosBagChoice = Nothing
    , chaosBagForceDraw = []
    , chaosBagTokenPool = []
    , chaosBagTotalRevealedChaosTokens = []
    , chaosBagPendingRequests = mempty
    , chaosBagDebugRevealedChaosTokens = []
    }

instance HasField "revealed" ChaosBag [ChaosToken] where
  getField = chaosBagRevealedChaosTokens

instance HasField "totalRevealed" ChaosBag [ChaosToken] where
  getField = chaosBagTotalRevealedChaosTokens

chaosTokensL :: Lens' ChaosBag [ChaosToken]
chaosTokensL = lens chaosBagChaosTokens $ \m x -> m {chaosBagChaosTokens = x}

tokenPoolL :: Lens' ChaosBag [ChaosToken]
tokenPoolL = lens chaosBagTokenPool $ \m x -> m {chaosBagTokenPool = x}

forceDrawL :: Lens' ChaosBag [ChaosTokenFace]
forceDrawL = lens chaosBagForceDraw $ \m x -> m {chaosBagForceDraw = x}

pendingRequestsL :: Lens' ChaosBag (Map Source [ChaosToken])
pendingRequestsL = lens chaosBagPendingRequests $ \m x -> m {chaosBagPendingRequests = x}

setAsideChaosTokensL :: Lens' ChaosBag [ChaosToken]
setAsideChaosTokensL =
  lens chaosBagSetAsideChaosTokens $ \m x -> m {chaosBagSetAsideChaosTokens = x}

revealedChaosTokensL :: Lens' ChaosBag [ChaosToken]
revealedChaosTokensL =
  lens chaosBagRevealedChaosTokens $ \m x -> m {chaosBagRevealedChaosTokens = x}

totalRevealedChaosTokensL :: Lens' ChaosBag [ChaosToken]
totalRevealedChaosTokensL =
  lens chaosBagTotalRevealedChaosTokens $ \m x -> m {chaosBagTotalRevealedChaosTokens = x}

debugRevealedChaosTokensL :: Lens' ChaosBag [DebugRevealedChaosToken]
debugRevealedChaosTokensL =
  lens chaosBagDebugRevealedChaosTokens $ \m x -> m {chaosBagDebugRevealedChaosTokens = x}

choiceL :: Lens' ChaosBag (Maybe ChaosBagStepState)
choiceL = lens chaosBagChoice $ \m x -> m {chaosBagChoice = x}
