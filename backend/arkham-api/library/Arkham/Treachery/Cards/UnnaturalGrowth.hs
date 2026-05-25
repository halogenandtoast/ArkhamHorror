module Arkham.Treachery.Cards.UnnaturalGrowth (unnaturalGrowth) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnnaturalGrowth = UnnaturalGrowth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unnaturalGrowth :: TreacheryCard UnnaturalGrowth
unnaturalGrowth = treachery UnnaturalGrowth Cards.unnaturalGrowth

instance RunMessage UnnaturalGrowth where
  runMessage msg t@(UnnaturalGrowth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ NearestEnemyTo iid $ EnemyWithoutDoom <> CanPlaceDoomOnEnemy
      if null enemies
        then randomDiscardN iid attrs 2
        else chooseTargetM iid enemies $ placeDoomOn attrs 1
      pure t
    _ -> UnnaturalGrowth <$> liftRunMessage msg attrs
