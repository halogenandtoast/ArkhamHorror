module Arkham.Helpers.FetchCard where

import Arkham.Asset.Types qualified as Field
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types qualified as Field
import Arkham.Event.Types qualified as Field
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Matcher.Card
import Arkham.Prelude
import Arkham.Projection
import Arkham.Treachery.Types qualified as Field
import Data.Monoid (First (..))

class FetchCard a where
  fetchCardMaybe :: (HasCallStack, HasGame m, CardGen m) => a -> m (Maybe Card)

fetchCard :: (HasCallStack, HasGame m, CardGen m, FetchCard a) => a -> m Card
fetchCard a = fromJustNote "Card not found" <$> fetchCardMaybe a

instance FetchCard UniqueFetchCard where
  fetchCardMaybe (UniqueFetchCard def) = do
    findCard ((== def.cardCode) . toCardCode) >>= \case
      Nothing -> Just <$> genCard def
      Just card -> pure $ Just $ if cardCodeExactEq def.cardCode card.cardCode then card else flipCard card

instance FetchCard CardDef where
  fetchCardMaybe def =
    if def.unique
      then fetchCardMaybe (UniqueFetchCard def)
      else maybe (Just <$> genCard def) (pure . Just) =<< maybeGetSetAsideCard def

newtype SetAsideCard = SetAsideCard CardDef

instance FetchCard SetAsideCard where
  fetchCardMaybe (SetAsideCard def) = maybeGetSetAsideCard def

instance FetchCard a => FetchCard [a] where
  fetchCardMaybe defs = getFirst . foldMap First <$> traverse fetchCardMaybe defs

instance FetchCard ExtendedCardMatcher where
  fetchCardMaybe = selectOne

instance FetchCard Card where
  fetchCardMaybe = pure . Just

instance FetchCard EncounterCard where
  fetchCardMaybe = pure . Just . toCard

instance FetchCard PlayerCard where
  fetchCardMaybe = pure . Just . toCard

instance FetchCard AssetId where
  fetchCardMaybe = fieldMap Field.AssetCard Just

instance FetchCard EventId where
  fetchCardMaybe = fieldMap Field.EventCard Just

instance FetchCard TreacheryId where
  fetchCardMaybe = fieldMap Field.TreacheryCard Just

instance FetchCard EnemyId where
  fetchCardMaybe = fieldMap Field.EnemyCard Just

instance FetchCard CardId where
  fetchCardMaybe = fmap Just . getCard

instance FetchCard Field.TreacheryAttrs where
  fetchCardMaybe = fieldMap Field.TreacheryCard Just . asId

newtype UniqueFetchCard = UniqueFetchCard CardDef
  deriving newtype (Show, Eq, ToJSON, FromJSON)
