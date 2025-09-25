module Arkham.Enemy.Cards.SenatorNathanielRhodesAdeptPolitician (senatorNathanielRhodesAdeptPolitician) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.AtDeathsDoorstep.Helpers

newtype SenatorNathanielRhodesAdeptPolitician = SenatorNathanielRhodesAdeptPolitician EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

senatorNathanielRhodesAdeptPolitician :: EnemyCard SenatorNathanielRhodesAdeptPolitician
senatorNathanielRhodesAdeptPolitician =
  enemy
    SenatorNathanielRhodesAdeptPolitician
    Cards.senatorNathanielRhodesAdeptPolitician
    (1, Static 2, 1)
    (0, 1)

instance HasAbilities SenatorNathanielRhodesAdeptPolitician where
  getAbilities (SenatorNathanielRhodesAdeptPolitician a) = extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage SenatorNathanielRhodesAdeptPolitician where
  runMessage msg e@(SenatorNathanielRhodesAdeptPolitician attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseOneM iid $ scenarioI18n do
        labeledValidate' (attrs.token #clue > 0) "senatorNathanielRhodesAdeptPolitician.take"
          $ moveTokens (attrs.ability 1) attrs iid #clue 1
        labeledValidate' (attrs.token #doom > 0) "senatorNathanielRhodesAdeptPolitician.flip"
          $ flipDoomToClues attrs 1
      pure e
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      flipCluesToDoom attrs 1
      pure e
    _ -> SenatorNathanielRhodesAdeptPolitician <$> liftRunMessage msg attrs
