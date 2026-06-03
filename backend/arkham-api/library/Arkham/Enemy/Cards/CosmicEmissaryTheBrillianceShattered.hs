module Arkham.Enemy.Cards.CosmicEmissaryTheBrillianceShattered (cosmicEmissaryTheBrillianceShattered) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Scenarios.FateOfTheVale.CosmicEmissary

newtype CosmicEmissaryTheBrillianceShattered = CosmicEmissaryTheBrillianceShattered EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheBrillianceShattered :: EnemyCard CosmicEmissaryTheBrillianceShattered
cosmicEmissaryTheBrillianceShattered =
  enemyWith CosmicEmissaryTheBrillianceShattered Cards.cosmicEmissaryTheBrillianceShattered (3, Static 10, 3) (2, 2)
    $ (asSelfLocationL ?~ "cosmicEmissaryBrilliance")
    . (preyL .~ Prey (InvestigatorWithHighestSkill #willpower UneliminatedInvestigator))

instance HasAbilities CosmicEmissaryTheBrillianceShattered where
  getAbilities (CosmicEmissaryTheBrillianceShattered attrs) = cosmicEmissaryShatteredAbility attrs #tablet

instance RunMessage CosmicEmissaryTheBrillianceShattered where
  runMessage msg e@(CosmicEmissaryTheBrillianceShattered attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resolveCosmicEmissaryShatteredAbility attrs iid
      pure e
    _ -> CosmicEmissaryTheBrillianceShattered <$> liftRunMessage msg attrs
