module Arkham.Enemy.Cards.MiGoGuard (miGoGuard) where

import Arkham.ChaosToken (ChaosTokenFace (..))
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTargetedEnemy, withSkillTest)
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Modifier

newtype MiGoGuard = MiGoGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoGuard :: EnemyCard MiGoGuard
miGoGuard =
  enemyWith MiGoGuard Cards.miGoGuard
    $ preyL
    .~ Prey MostRemainingHealth

-- For each [skull], [cultist], [tablet], or [elder_thing] token revealed
-- during an attack against Mi-Go Guard, that attack deals 1 fewer damage.
instance RunMessage MiGoGuard where
  runMessage msg e@(MiGoGuard attrs) = runQueueT $ case msg of
    After (RevealChaosToken _ iid token) | token.face `elem` [Skull, Cultist, Tablet, ElderThing] -> do
      whenM ((== Just #fight) <$> getSkillTestAction) do
        getSkillTestTargetedEnemy >>= traverse_ \eid ->
          when (eid == attrs.id) do
            withSkillTest \sid ->
              skillTestModifier sid (toSource attrs) iid (DamageDealt (-1))
      pure e
    _ -> MiGoGuard <$> liftRunMessage msg attrs
