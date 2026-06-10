module Arkham.Act.Cards.TheHourOfJudgment (theHourOfJudgment) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose

newtype TheHourOfJudgment = TheHourOfJudgment ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHourOfJudgment :: ActCard TheHourOfJudgment
theHourOfJudgment = act (3, A) TheHourOfJudgment Cards.theHourOfJudgment Nothing

instance HasAbilities TheHourOfJudgment where
  getAbilities (TheHourOfJudgment a) =
    [ skillTestAbility
        $ restricted a 1 (youExist $ InvestigatorAt $ LocationWithDamage $ atLeast 1) actionAbility
    , mkAbility a 2
        $ Objective
        $ forced
        $ Matcher.EnemyDefeated #after Anyone ByAny (enemyIs Enemies.neith)
    ]

instance RunMessage TheHourOfJudgment where
  runMessage msg a@(TheHourOfJudgment attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      n <- getStrengthOfTheAbyss
      withI18n $ chooseOneM iid do
        labeled' "chooseWillpower" $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed n)
        labeled' "chooseIntellect" $ beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed n)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withLocationOf iid \loc -> do
        whenJustM (selectOne $ enemyIs Enemies.neith) \neith -> do
          removeTokens (attrs.ability 1) loc #damage 1
          nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 neith
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      campaignI18n $ chooseOneM lead do
        labeled' "theHourOfJudgment.destroyNeith" $ push R1
        labeled' "theHourOfJudgment.spareNeith" $ push R2
      pure a
    _ -> TheHourOfJudgment <$> liftRunMessage msg attrs
