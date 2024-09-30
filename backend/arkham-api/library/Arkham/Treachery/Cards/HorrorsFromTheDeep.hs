module Arkham.Treachery.Cards.HorrorsFromTheDeep (
  horrorsFromTheDeep,
  HorrorsFromTheDeep (..),
)
where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Helpers.Location
import Arkham.Location.FloodLevel
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HorrorsFromTheDeep = HorrorsFromTheDeep TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrorsFromTheDeep :: TreacheryCard HorrorsFromTheDeep
horrorsFromTheDeep = treachery HorrorsFromTheDeep Cards.horrorsFromTheDeep

instance RunMessage HorrorsFromTheDeep where
  runMessage msg t@(HorrorsFromTheDeep attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      extra <-
        getLocationOf iid >>= \case
          Nothing -> pure 0
          Just loc -> do
            getFloodLevel loc <&> \case
              Unflooded -> 0
              PartiallyFlooded -> 1
              FullyFlooded -> 2
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed $ 2 + extra)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> HorrorsFromTheDeep <$> liftRunMessage msg attrs
