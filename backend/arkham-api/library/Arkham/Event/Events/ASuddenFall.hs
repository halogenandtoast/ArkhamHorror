module Arkham.Event.Events.ASuddenFall (aSuddenFall) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Modifier

newtype ASuddenFall = ASuddenFall EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aSuddenFall :: EventCard ASuddenFall
aSuddenFall = event ASuddenFall Cards.aSuddenFall

instance RunMessage ASuddenFall where
  runMessage msg e@(ASuddenFall attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (SkillModifier #combat 2)
      chooseFightEnemy sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withSkillTest \sid ->
        withSkillTestTargetedEnemy \enemy -> do
          whenMatch enemy ExhaustedEnemy do
            priority $ skillTestModifier sid attrs iid (DamageDealt 1)
      pure e
    _ -> ASuddenFall <$> liftRunMessage msg attrs
