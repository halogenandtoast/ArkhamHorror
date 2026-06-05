module Arkham.Enemy.Cards.CosmicEmissaryTheMiasmaShattered (cosmicEmissaryTheMiasmaShattered) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Scenarios.FateOfTheVale.CosmicEmissary

newtype CosmicEmissaryTheMiasmaShattered = CosmicEmissaryTheMiasmaShattered EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheMiasmaShattered :: EnemyCard CosmicEmissaryTheMiasmaShattered
cosmicEmissaryTheMiasmaShattered =
  enemyWith CosmicEmissaryTheMiasmaShattered Cards.cosmicEmissaryTheMiasmaShattered (4, Static 10, 2) (3, 0)
    $ (asSelfLocationL ?~ "cosmicEmissaryMiasma")
    . (preyL .~ Prey (InvestigatorWithHighestSkill #agility UneliminatedInvestigator))

instance HasAbilities CosmicEmissaryTheMiasmaShattered where
  getAbilities (CosmicEmissaryTheMiasmaShattered attrs) = cosmicEmissaryShatteredAbility attrs #cultist

instance RunMessage CosmicEmissaryTheMiasmaShattered where
  runMessage msg e@(CosmicEmissaryTheMiasmaShattered attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resolveCosmicEmissaryShatteredAbility attrs iid
      pure e
    _ -> CosmicEmissaryTheMiasmaShattered <$> liftRunMessage msg attrs
