module Arkham.Homebrew.CircusExMortis.Enemies.NewMoonAcrobat (newMoonAcrobat) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype NewMoonAcrobat = NewMoonAcrobat EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newMoonAcrobat :: EnemyCard NewMoonAcrobat
newMoonAcrobat = enemy NewMoonAcrobat Cards.newMoonAcrobat

instance HasModifiersFor NewMoonAcrobat where
  getModifiersFor (NewMoonAcrobat a) = modifySelf a [AddKeyword Keyword.Hunter]

instance HasAbilities NewMoonAcrobat where
  getAbilities (NewMoonAcrobat a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      -- "before your attack resolves" -> #when fires at result, prior to fight damage.
      $ SkillTestResult #when You (WhileAttackingAnEnemy $ be a) (SuccessResult $ atLeast 2)

instance RunMessage NewMoonAcrobat where
  runMessage msg e@(NewMoonAcrobat attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> NewMoonAcrobat <$> liftRunMessage msg attrs
