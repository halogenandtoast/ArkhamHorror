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
  enemyWith CosmicEmissaryTheMiasma Cards.cosmicEmissaryTheMiasma (4, Static 0, 2) (2, 0)
    $ (healthL .~ Nothing)
    . (asSelfLocationL ?~ "cosmicEmissaryMiasma")

instance HasModifiersFor CosmicEmissaryTheMiasma where
  getModifiersFor (CosmicEmissaryTheMiasma attrs) = modifySelf attrs [CannotMakeAttacksOfOpportunity, CannotBeDamaged]

instance HasAbilities CosmicEmissaryTheMiasma where
  getAbilities (CosmicEmissaryTheMiasma attrs) = cosmicEmissaryColourAbilities attrs

instance RunMessage CosmicEmissaryTheMiasma where
  runMessage msg e@(CosmicEmissaryTheMiasma attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) n windows _ | n `elem` [1, 2] -> do
      handleCosmicEmissaryColour attrs n windows
      pure e
    _ -> CosmicEmissaryTheMiasma <$> liftRunMessage msg attrs
