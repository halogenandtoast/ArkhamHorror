module Arkham.Treachery.Cards.MacabreMemento (macabreMemento, MacabreMemento (..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MacabreMemento = MacabreMemento TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

macabreMemento :: TreacheryCard MacabreMemento
macabreMemento = treachery MacabreMemento Cards.macabreMemento

instance RunMessage MacabreMemento where
  runMessage msg t@(MacabreMemento attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #cultist attrs attrs failSkillTest
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> MacabreMemento <$> liftRunMessage msg attrs
