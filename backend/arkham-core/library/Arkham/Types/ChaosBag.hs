module Arkham.Types.ChaosBag
  ( ChaosBag
  , emptyChaosBag
  )
where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Token
import qualified Data.List as L
import Lens.Micro
import System.Random.Shuffle

data ChaosBag = ChaosBag
  { chaosBagTokens :: [Token]
  , chaosBagSetAsideTokens :: [Token]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ChaosBag where
  toJSON = genericToJSON $ aesonOptions $ Just "chaosBag"
  toEncoding = genericToEncoding $ aesonOptions $ Just "chaosBag"

instance FromJSON ChaosBag where
  parseJSON = genericParseJSON $ aesonOptions $ Just "chaosBag"

emptyChaosBag :: ChaosBag
emptyChaosBag = ChaosBag { chaosBagTokens = [], chaosBagSetAsideTokens = [] }

tokens :: Lens' ChaosBag [Token]
tokens = lens chaosBagTokens $ \m x -> m { chaosBagTokens = x }

setAsideTokens :: Lens' ChaosBag [Token]
setAsideTokens =
  lens chaosBagSetAsideTokens $ \m x -> m { chaosBagSetAsideTokens = x }

instance HasQueue env => RunMessage env ChaosBag where
  runMessage msg c@ChaosBag {..} = case msg of
    SetTokens tokens' ->
      pure $ c & tokens .~ tokens' & setAsideTokens .~ mempty
    ResetTokens ->
      pure
        $ c
        & tokens
        %~ (<> chaosBagSetAsideTokens)
        & setAsideTokens
        .~ mempty
    RequestTokens source iid n strategy -> do
      (drawn, remaining) <- splitAt n <$> liftIO (shuffleM chaosBagTokens)
      unshiftMessage (RequestedTokens source iid drawn)
      case strategy of
        SetAside -> pure $ c & tokens .~ remaining & setAsideTokens .~ drawn
        RemoveTokens -> pure $ c & tokens .~ remaining
    ReturnTokens tokens' ->
      pure $ c & tokens %~ (<> tokens') & setAsideTokens %~ (L.\\ tokens')
    AddToken token -> pure $ c & tokens %~ (token :)
    _ -> pure c
