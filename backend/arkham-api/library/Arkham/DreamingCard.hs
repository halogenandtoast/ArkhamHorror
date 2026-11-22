{-# LANGUAGE TemplateHaskell #-}

module Arkham.DreamingCard where

import Arkham.Card
import Arkham.Json
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Data.Aeson.TH
import GHC.Records

{- | A card in an investigator's dream zone: played facedown, counting down its
dreams. Deliberately a record and not an entity -- a facedown dreaming card is
not in play, so no 'HasAbilities' and no 'HasModifiersFor' may run on one.

Dreams are 'Dream' tokens in 'dreamingCardTokens'. The list on the investigator
is append-only and removed by 'CardId'; see 'Arkham.Investigator.Types'.
-}
data DreamingCard = DreamingCard
  { dreamingCardCard :: Card
  , dreamingCardTokens :: Tokens
  }
  deriving stock (Show, Eq, Ord, Data)

instance HasField "card" DreamingCard Card where
  getField = dreamingCardCard

instance HasField "tokens" DreamingCard Tokens where
  getField = dreamingCardTokens

instance HasField "id" DreamingCard CardId where
  getField = toCardId . dreamingCardCard

instance HasField "dreams" DreamingCard Int where
  getField = countTokens Dream . dreamingCardTokens

instance Targetable DreamingCard where
  toTarget = DreamingCardTarget . toCardId . dreamingCardCard

instance Sourceable DreamingCard where
  toSource = DreamingCardSource . toCardId . dreamingCardCard

$(deriveToJSON (aesonOptions $ Just "dreamingCard") ''DreamingCard)

{- | Hand-rolled: 'tokens' is optional so a record written before the field grew
still decodes, and this type is expected to gain fields.
-}
instance FromJSON DreamingCard where
  parseJSON = withObject "DreamingCard" \o -> do
    dreamingCardCard <- o .: "card"
    dreamingCardTokens <- o .:? "tokens" .!= mempty
    pure DreamingCard {..}

dreamingCardWith :: Card -> Int -> DreamingCard
dreamingCardWith card n = DreamingCard card (addTokens Dream n mempty)
