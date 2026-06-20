module Arkham.Enemy.Cards.CrystalMimic (crystalMimic) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isEvadeWith, isFightWith)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype CrystalMimic = CrystalMimic EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalMimic :: EnemyCard CrystalMimic
crystalMimic = enemy CrystalMimic Cards.crystalMimic

instance HasModifiersFor CrystalMimic where
  getModifiersFor (CrystalMimic attrs) = do
    fighting <- isFightWith (be attrs)
    evading <- isEvadeWith (be attrs)
    when (fighting || evading) do
      miid <- getSkillTestInvestigator
      for_ miid \iid -> do
        handCount <- fieldMap InvestigatorHand length iid
        modified_ attrs attrs [EnemyFight (handCount - 6), EnemyEvade (handCount - 6)]

instance HasAbilities CrystalMimic where
  getAbilities (CrystalMimic a) = extend1 a $ forcedAbility a 1 $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage CrystalMimic where
  runMessage msg e@(CrystalMimic attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscard iid (attrs.ability 1)
      pure e
    _ -> CrystalMimic <$> liftRunMessage msg attrs
