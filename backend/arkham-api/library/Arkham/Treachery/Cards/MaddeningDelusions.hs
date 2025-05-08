module Arkham.Treachery.Cards.MaddeningDelusions (maddeningDelusions) where

import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MaddeningDelusions = MaddeningDelusions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maddeningDelusions :: TreacheryCard MaddeningDelusions
maddeningDelusions = treachery MaddeningDelusions Cards.maddeningDelusions

instance RunMessage MaddeningDelusions where
  runMessage msg t@(MaddeningDelusions attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasHiddenTreachery <- selectAny $ TreacheryWithPlacement (HiddenInHand iid)
      hasHiddenEnemy <- selectAny $ EnemyWithPlacement (HiddenInHand iid)
      when (hasHiddenEnemy || hasHiddenTreachery) $ assignHorror iid attrs 1
      pure t
    _ -> MaddeningDelusions <$> liftRunMessage msg attrs
