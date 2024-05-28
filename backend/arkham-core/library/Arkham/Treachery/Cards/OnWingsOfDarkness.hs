module Arkham.Treachery.Cards.OnWingsOfDarkness where

import Arkham.Classes
import Arkham.Game.Helpers (getCanMoveToMatchingLocations)
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude
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
      revelationSkillTest iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      centralLocations <- getCanMoveToMatchingLocations iid attrs $ LocationWithTrait Central
      enemiesToDisengage <- select $ enemyEngagedWith iid <> EnemyWithoutTrait Nightgaunt
      assignDamageAndHorror iid attrs 1 1
      pushAll $ map (DisengageEnemy iid) enemiesToDisengage
      when (notNull centralLocations) do
        chooseOne iid $ targetLabels centralLocations (only . MoveTo . move attrs iid)
      pure t
    _ -> OnWingsOfDarkness <$> lift (runMessage msg attrs)
