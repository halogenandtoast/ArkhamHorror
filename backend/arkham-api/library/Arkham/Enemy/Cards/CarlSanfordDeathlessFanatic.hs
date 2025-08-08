module Arkham.Enemy.Cards.CarlSanfordDeathlessFanatic (carlSanfordDeathlessFanatic) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher

newtype CarlSanfordDeathlessFanatic = CarlSanfordDeathlessFanatic EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carlSanfordDeathlessFanatic :: EnemyCard CarlSanfordDeathlessFanatic
carlSanfordDeathlessFanatic =
  enemy CarlSanfordDeathlessFanatic Cards.carlSanfordDeathlessFanatic (4, PerPlayer 6, 4) (1, 3)
    & setSpawnAt "Silver Twilight Lodge"

instance HasModifiersFor CarlSanfordDeathlessFanatic where
  getModifiersFor (CarlSanfordDeathlessFanatic a) = do
    clues <- getSum <$> selectAgg Sum InvestigatorClues UneliminatedInvestigator
    modifySelf a [HealthModifier $ negate $ 2 * clues]

instance HasAbilities CarlSanfordDeathlessFanatic where
  getAbilities (CarlSanfordDeathlessFanatic a) =
    extend1 a $ forcedAbility a 1 $ RemovedBreaches #after $ ActTargetMatches AnyAct

instance RunMessage CarlSanfordDeathlessFanatic where
  runMessage msg e@(CarlSanfordDeathlessFanatic attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \location ->
        push $ PlaceBreaches (toTarget location) 1
      pure e
    After (GainClues {}) -> do
      CarlSanfordDeathlessFanatic <$> liftRunMessage (CheckDefeated GameSource (toTarget attrs)) attrs
    _ -> CarlSanfordDeathlessFanatic <$> liftRunMessage msg attrs
