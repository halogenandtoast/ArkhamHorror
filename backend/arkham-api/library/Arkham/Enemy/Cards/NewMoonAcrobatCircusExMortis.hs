module Arkham.Enemy.Cards.NewMoonAcrobatCircusExMortis (newMoonAcrobatCircusExMortis) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype NewMoonAcrobatCircusExMortis = NewMoonAcrobatCircusExMortis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newMoonAcrobatCircusExMortis :: EnemyCard NewMoonAcrobatCircusExMortis
newMoonAcrobatCircusExMortis = enemy NewMoonAcrobatCircusExMortis Cards.newMoonAcrobatCircusExMortis

instance HasModifiersFor NewMoonAcrobatCircusExMortis where
  getModifiersFor (NewMoonAcrobatCircusExMortis a) = modifySelf a [AddKeyword Keyword.Hunter]

instance HasAbilities NewMoonAcrobatCircusExMortis where
  getAbilities (NewMoonAcrobatCircusExMortis a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      -- "before your attack resolves" -> #when fires at result, prior to fight damage.
      $ SkillTestResult #when You (WhileAttackingAnEnemy $ be a) (SuccessResult $ atLeast 2)

instance RunMessage NewMoonAcrobatCircusExMortis where
  runMessage msg e@(NewMoonAcrobatCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> NewMoonAcrobatCircusExMortis <$> liftRunMessage msg attrs
