module Arkham.Treachery.Cards.IceShaft (iceShaft, IceShaft (..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype IceShaft = IceShaft TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iceShaft :: TreacheryCard IceShaft
iceShaft = treachery IceShaft Cards.iceShaft

instance RunMessage IceShaft where
  runMessage msg t@(IceShaft attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #frost attrs iid do
        assignDamage iid attrs 1
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> IceShaft <$> liftRunMessage msg attrs
