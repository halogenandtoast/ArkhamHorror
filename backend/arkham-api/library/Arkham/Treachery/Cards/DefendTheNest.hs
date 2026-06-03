module Arkham.Treachery.Cards.DefendTheNest (defendTheNest) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Insect, Lair))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DefendTheNest = DefendTheNest TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defendTheNest :: TreacheryCard DefendTheNest
defendTheNest = treachery DefendTheNest Cards.defendTheNest

instance RunMessage DefendTheNest where
  runMessage msg t@(DefendTheNest attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect
        $ SumCalculation [Fixed 2, CountLocations (LocationWithTrait Lair)]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      insects <- select $ NearestEnemyToFallback iid (EnemyWithTrait Insect <> CanPlaceDoomOnEnemy)
      if null insects
        then assignHorror iid attrs 2
        else chooseTargetM iid insects \enemy -> placeDoom attrs enemy 1
      pure t
    _ -> DefendTheNest <$> liftRunMessage msg attrs
