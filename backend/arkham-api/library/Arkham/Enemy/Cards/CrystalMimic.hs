module Arkham.Enemy.Cards.CrystalMimic (crystalMimic) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isEvading, isFighting)
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
    fighting <- isFighting attrs
    evading <- isEvading attrs
    miid <- getSkillTestInvestigator
    case (fighting || evading, miid) of
      (True, Just iid) -> do
        handCount <- fieldMap InvestigatorHand length iid
        modifySelf attrs [EnemyFight handCount, EnemyEvade handCount]
      _ -> modifySelf attrs [EnemyFight 6, EnemyEvade 6]

instance HasAbilities CrystalMimic where
  getAbilities (CrystalMimic a) = extend1 a $ forcedAbility a 1 $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage CrystalMimic where
  runMessage msg e@(CrystalMimic attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscard iid (attrs.ability 1)
      pure e
    _ -> CrystalMimic <$> liftRunMessage msg attrs
