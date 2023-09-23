module Arkham.Treachery.Cards.OnWingsOfDarkness where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype OnWingsOfDarkness = OnWingsOfDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onWingsOfDarkness :: TreacheryCard OnWingsOfDarkness
onWingsOfDarkness = treachery OnWingsOfDarkness Cards.onWingsOfDarkness

instance RunMessage OnWingsOfDarkness where
  runMessage msg t@(OnWingsOfDarkness attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #agility 4
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      centralLocations <- selectList $ LocationWithTrait Central
      enemiesToDisengage <- selectList $ enemyEngagedWith iid <> EnemyWithoutTrait Nightgaunt
      pushAll
        $ assignDamageAndHorror iid attrs 1 1
        : map (DisengageEnemy iid) enemiesToDisengage
          <> [ chooseOne
                iid
                [ targetLabel lid [MoveTo $ move (toSource attrs) iid lid]
                | lid <- centralLocations
                ]
             ]
      pure t
    _ -> OnWingsOfDarkness <$> runMessage msg attrs
