module Arkham.Enemy.Cards.CosmicEmissaryTheMiasma (cosmicEmissaryTheMiasma) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Scenarios.FateOfTheVale.CosmicEmissary

newtype CosmicEmissaryTheMiasma = CosmicEmissaryTheMiasma EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheMiasma :: EnemyCard CosmicEmissaryTheMiasma
cosmicEmissaryTheMiasma =
  enemyWith CosmicEmissaryTheMiasma Cards.cosmicEmissaryTheMiasma
    $ (healthL .~ Nothing)
    . (asSelfLocationL ?~ "cosmicEmissaryMiasma")

instance HasModifiersFor CosmicEmissaryTheMiasma where
  getModifiersFor (CosmicEmissaryTheMiasma attrs) = modifySelf attrs [CannotMakeAttacksOfOpportunity, CannotBeDamaged, DoNotExhaustEvaded]

instance HasAbilities CosmicEmissaryTheMiasma where
  getAbilities (CosmicEmissaryTheMiasma attrs) = cosmicEmissaryColourAbilities attrs

instance RunMessage CosmicEmissaryTheMiasma where
  runMessage msg e@(CosmicEmissaryTheMiasma attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows _ -> do
      handleCosmicEmissaryColour attrs windows
      pure e
    _ -> CosmicEmissaryTheMiasma <$> liftRunMessage msg attrs
