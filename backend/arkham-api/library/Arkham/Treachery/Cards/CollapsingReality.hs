module Arkham.Treachery.Cards.CollapsingReality (collapsingReality) where

import Arkham.Matcher
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CollapsingReality = CollapsingReality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collapsingReality :: TreacheryCard CollapsingReality
collapsingReality = treachery CollapsingReality Cards.collapsingReality

instance RunMessage CollapsingReality where
  runMessage msg t@(CollapsingReality attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectOne (locationWithInvestigator iid <> withTrait Extradimensional) >>= \case
        Nothing -> assignDamage iid attrs 2
        Just lid -> do
          toDiscardBy iid attrs lid
          assignDamage iid attrs 1
      pure t
    _ -> CollapsingReality <$> liftRunMessage msg attrs
