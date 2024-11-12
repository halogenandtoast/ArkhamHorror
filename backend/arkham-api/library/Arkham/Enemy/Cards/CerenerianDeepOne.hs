module Arkham.Enemy.Cards.CerenerianDeepOne (cerenerianDeepOne, CerenerianDeepOne (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Matcher

newtype CerenerianDeepOne = CerenerianDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cerenerianDeepOne :: EnemyCard CerenerianDeepOne
cerenerianDeepOne =
  enemyWith CerenerianDeepOne Cards.cerenerianDeepOne (2, Static 3, 2) (1, 1)
    $ (preyL .~ Prey (InvestigatorWithLowestSkill #willpower UneliminatedInvestigator))

instance HasAbilities CerenerianDeepOne where
  getAbilities (CerenerianDeepOne a) = extend1 a $ restricted a 1 HasRemainingCurseTokens $ forced $ EnemyEngaged #after You (be a)

instance RunMessage CerenerianDeepOne where
  runMessage msg e@(CerenerianDeepOne attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      n <- min 1 <$> getRemainingCurseTokens
      repeated n $ addChaosToken #curse
      pure e
    _ -> CerenerianDeepOne <$> liftRunMessage msg attrs
