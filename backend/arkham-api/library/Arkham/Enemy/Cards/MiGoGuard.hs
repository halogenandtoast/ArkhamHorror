module Arkham.Enemy.Cards.MiGoGuard (miGoGuard) where

import Arkham.Action qualified as Action
import Arkham.ChaosToken (ChaosTokenFace (..))
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestId, getSkillTestTargetedEnemy)
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Modifier

newtype MiGoGuard = MiGoGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

miGoGuard :: EnemyCard MiGoGuard
miGoGuard = enemy MiGoGuard Cards.miGoGuard & setPrey MostRemainingHealth

-- For each [skull], [cultist], [tablet], or [elder_thing] token revealed
-- during an attack against Mi-Go Guard, that attack deals 1 fewer damage.
instance RunMessage MiGoGuard where
  runMessage msg e@(MiGoGuard attrs) = runQueueT $ case msg of
    After (RevealChaosToken _ iid token) | token.face `elem` [Skull, Cultist, Tablet, ElderThing] -> do
      runMaybeT_ do
        Action.Fight <- MaybeT getSkillTestAction
        eid <- MaybeT getSkillTestTargetedEnemy
        guard $ eid == attrs.id
        sid <- MaybeT getSkillTestId
        lift $ skillTestModifier sid (toSource attrs) iid (DamageDealt (-1))
      pure e
    _ -> MiGoGuard <$> liftRunMessage msg attrs
