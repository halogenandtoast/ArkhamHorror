module Arkham.ChaosBag.Base where

import Arkham.Prelude

import Arkham.ChaosBagStepState
import Arkham.Json
import Arkham.Token

data ChaosBag = ChaosBag
  { chaosBagTokens :: [Token]
  , chaosBagSetAsideTokens :: [Token]
  , chaosBagRevealedTokens :: [Token]
  , chaosBagChoice :: Maybe ChaosBagStepState
  , chaosBagForceDraw :: Maybe TokenFace
  }
  deriving stock (Show, Eq, Generic)

allChaosBagTokens :: ChaosBag -> [Token]
allChaosBagTokens ChaosBag {..} =
  chaosBagTokens <> chaosBagSetAsideTokens <> chaosBagRevealedTokens

instance ToJSON ChaosBag where
  toJSON = genericToJSON $ aesonOptions $ Just "chaosBag"
  toEncoding = genericToEncoding $ aesonOptions $ Just "chaosBag"

instance FromJSON ChaosBag where
  parseJSON = genericParseJSON $ aesonOptions $ Just "chaosBag"

emptyChaosBag :: ChaosBag
emptyChaosBag = ChaosBag
  { chaosBagTokens = []
  , chaosBagSetAsideTokens = []
  , chaosBagRevealedTokens = []
  , chaosBagChoice = Nothing
  , chaosBagForceDraw = Nothing
  }

tokensL :: Lens' ChaosBag [Token]
tokensL = lens chaosBagTokens $ \m x -> m { chaosBagTokens = x }

forceDrawL :: Lens' ChaosBag (Maybe TokenFace)
forceDrawL = lens chaosBagForceDraw $ \m x -> m { chaosBagForceDraw = x }

setAsideTokensL :: Lens' ChaosBag [Token]
setAsideTokensL =
  lens chaosBagSetAsideTokens $ \m x -> m { chaosBagSetAsideTokens = x }

revealedTokensL :: Lens' ChaosBag [Token]
revealedTokensL =
  lens chaosBagRevealedTokens $ \m x -> m { chaosBagRevealedTokens = x }

choiceL :: Lens' ChaosBag (Maybe ChaosBagStepState)
choiceL = lens chaosBagChoice $ \m x -> m { chaosBagChoice = x }

createToken :: MonadRandom m => TokenFace -> m Token
createToken face = Token <$> getRandom <*> pure face

