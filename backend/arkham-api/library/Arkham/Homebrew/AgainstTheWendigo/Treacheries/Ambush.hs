module Arkham.Homebrew.AgainstTheWendigo.Treacheries.Ambush (ambush) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Ambush = Ambush TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ambush :: TreacheryCard Ambush
ambush = treachery Ambush Cards.ambush

instance RunMessage Ambush where
  runMessage msg t@(Ambush attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      pure t
    _ -> Ambush <$> liftRunMessage msg attrs
