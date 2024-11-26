module Arkham.Treachery.Cards.DarkAurora (darkAurora, DarkAurora (..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DarkAurora = DarkAurora TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkAurora :: TreacheryCard DarkAurora
darkAurora = treachery DarkAurora Cards.darkAurora

instance RunMessage DarkAurora where
  runMessage msg t@(DarkAurora attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #frost attrs iid do
        assignHorror iid attrs 1
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> DarkAurora <$> liftRunMessage msg attrs
