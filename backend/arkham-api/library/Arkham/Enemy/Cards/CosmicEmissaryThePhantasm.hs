module Arkham.Enemy.Cards.CosmicEmissaryThePhantasm (cosmicEmissaryThePhantasm) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Scenarios.FateOfTheVale.CosmicEmissary

newtype CosmicEmissaryThePhantasm = CosmicEmissaryThePhantasm EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryThePhantasm :: EnemyCard CosmicEmissaryThePhantasm
cosmicEmissaryThePhantasm =
  enemyWith CosmicEmissaryThePhantasm Cards.cosmicEmissaryThePhantasm (2, Static 0, 4) (1, 1)
    $ (healthL .~ Nothing)
    . (asSelfLocationL ?~ "cosmicEmissaryPhantasm")

instance HasModifiersFor CosmicEmissaryThePhantasm where
  getModifiersFor (CosmicEmissaryThePhantasm attrs) = modifySelf attrs [CannotMakeAttacksOfOpportunity, CannotBeDamaged, DoNotExhaustEvaded]

instance HasAbilities CosmicEmissaryThePhantasm where
  getAbilities (CosmicEmissaryThePhantasm attrs) = cosmicEmissaryColourAbilities attrs

instance RunMessage CosmicEmissaryThePhantasm where
  runMessage msg e@(CosmicEmissaryThePhantasm attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows _ -> do
      handleCosmicEmissaryColour attrs windows
      pure e
    _ -> CosmicEmissaryThePhantasm <$> liftRunMessage msg attrs
