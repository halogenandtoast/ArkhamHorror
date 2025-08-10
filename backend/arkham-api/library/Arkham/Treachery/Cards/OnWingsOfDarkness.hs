module Arkham.Treachery.Cards.OnWingsOfDarkness (onWingsOfDarkness) where

import Arkham.Helpers.Location (getCanMoveToMatchingLocations)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OnWingsOfDarkness = OnWingsOfDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onWingsOfDarkness :: TreacheryCard OnWingsOfDarkness
onWingsOfDarkness = treachery OnWingsOfDarkness Cards.onWingsOfDarkness

instance RunMessage OnWingsOfDarkness where
  runMessage msg t@(OnWingsOfDarkness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamageAndHorror iid attrs 1 1
      enemiesToDisengage <- select $ enemyEngagedWith iid <> EnemyWithoutTrait Nightgaunt
      for_ enemiesToDisengage (disengageEnemy iid)
      centralLocations <- getCanMoveToMatchingLocations iid attrs $ LocationWithTrait Central
      chooseTargetM iid centralLocations (moveTo attrs iid)
      for_ enemiesToDisengage enemyCheckEngagement
      pure t
    _ -> OnWingsOfDarkness <$> liftRunMessage msg attrs
