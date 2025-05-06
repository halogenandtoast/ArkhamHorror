module Arkham.Treachery.Cards.EphemeralExhibits (ephemeralExhibits) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EphemeralExhibits = EphemeralExhibits TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ephemeralExhibits :: TreacheryCard EphemeralExhibits
ephemeralExhibits = treachery EphemeralExhibits Cards.ephemeralExhibits

instance RunMessage EphemeralExhibits where
  runMessage msg t@(EphemeralExhibits attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      loseActions iid attrs n
      pure t
    _ -> EphemeralExhibits <$> liftRunMessage msg attrs
