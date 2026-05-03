module Arkham.Event.Events.ASuddenFall2 (aSuddenFall2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Modifier

newtype ASuddenFall2 = ASuddenFall2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aSuddenFall2 :: EventCard ASuddenFall2
aSuddenFall2 = event ASuddenFall2 Cards.aSuddenFall2

instance RunMessage ASuddenFall2 where
  runMessage msg e@(ASuddenFall2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #combat)
      chooseFightEnemyWith #agility sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withSkillTest \sid ->
        withSkillTestTargetedEnemy \enemy -> do
          whenMatch enemy ExhaustedEnemy do
            priority $ skillTestModifier sid attrs iid (DamageDealt 2)
      pure e
    _ -> ASuddenFall2 <$> liftRunMessage msg attrs
