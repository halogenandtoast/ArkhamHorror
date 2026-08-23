module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.JavierRivera (javierRivera) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype JavierRivera = JavierRivera EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

javierRivera :: EnemyCard JavierRivera
javierRivera = enemy JavierRivera Cards.javierRivera

instance HasModifiersFor JavierRivera where
  getModifiersFor (JavierRivera a) = whenJustM getSkillTest \st ->
    when (st.action `elem` [Just #investigate, Just #parley]) do
      here <- st.investigator <=~> InvestigatorAt (locationWithEnemy a.id)
      modifiedWhen_ a here st [Difficulty 2]

instance HasAbilities JavierRivera where
  getAbilities (JavierRivera a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage JavierRivera where
  runMessage msg e@(JavierRivera attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      when attrs.exhausted
        $ skillTestModifier sid (attrs.ability 1) sid SkillTestAutomaticallySucceeds
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 4)
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember TheInvestigatorsStoleTheManagersKeys
      pure e
    _ -> JavierRivera <$> liftRunMessage msg attrs
