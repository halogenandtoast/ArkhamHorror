module Arkham.Treachery.Cards.AlienEggs (alienEggs) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AlienEggs = AlienEggs TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienEggs :: TreacheryCard AlienEggs
alienEggs = treachery AlienEggs Cards.alienEggs

instance RunMessage AlienEggs where
  runMessage msg t@(AlienEggs attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ NearestEnemyTo iid $ NonEliteEnemy <> EnemyWithoutDoom <> CanPlaceDoomOnEnemy
      unless (null enemies) $ chooseTargetM iid enemies $ placeDoomOn attrs 1
      gainSurge attrs
      pure t
    _ -> AlienEggs <$> liftRunMessage msg attrs
