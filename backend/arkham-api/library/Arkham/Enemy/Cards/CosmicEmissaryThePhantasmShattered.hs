module Arkham.Enemy.Cards.CosmicEmissaryThePhantasmShattered (cosmicEmissaryThePhantasmShattered) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Scenarios.FateOfTheVale.CosmicEmissary

newtype CosmicEmissaryThePhantasmShattered = CosmicEmissaryThePhantasmShattered EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryThePhantasmShattered :: EnemyCard CosmicEmissaryThePhantasmShattered
cosmicEmissaryThePhantasmShattered =
  enemyWith CosmicEmissaryThePhantasmShattered Cards.cosmicEmissaryThePhantasmShattered
    $ (asSelfLocationL ?~ "cosmicEmissaryPhantasm")
    . (preyL .~ Prey (InvestigatorWithHighestSkill #combat UneliminatedInvestigator))

instance HasAbilities CosmicEmissaryThePhantasmShattered where
  getAbilities (CosmicEmissaryThePhantasmShattered attrs) = cosmicEmissaryShatteredAbility attrs #elderthing

instance RunMessage CosmicEmissaryThePhantasmShattered where
  runMessage msg e@(CosmicEmissaryThePhantasmShattered attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resolveCosmicEmissaryShatteredAbility attrs iid
      pure e
    _ -> CosmicEmissaryThePhantasmShattered <$> liftRunMessage msg attrs
