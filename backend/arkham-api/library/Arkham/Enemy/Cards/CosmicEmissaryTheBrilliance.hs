module Arkham.Enemy.Cards.CosmicEmissaryTheBrilliance (cosmicEmissaryTheBrilliance) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Scenarios.FateOfTheVale.CosmicEmissary

newtype CosmicEmissaryTheBrilliance = CosmicEmissaryTheBrilliance EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheBrilliance :: EnemyCard CosmicEmissaryTheBrilliance
cosmicEmissaryTheBrilliance =
  enemyWith CosmicEmissaryTheBrilliance Cards.cosmicEmissaryTheBrilliance (3, Static 0, 3) (1, 1)
    $ (healthL .~ Nothing)
    . (asSelfLocationL ?~ "cosmicEmissaryBrilliance")

instance HasModifiersFor CosmicEmissaryTheBrilliance where
  getModifiersFor (CosmicEmissaryTheBrilliance attrs) = modifySelf attrs [CannotMakeAttacksOfOpportunity, CannotBeDamaged]

instance HasAbilities CosmicEmissaryTheBrilliance where
  getAbilities (CosmicEmissaryTheBrilliance attrs) = cosmicEmissaryColourAbilities attrs

instance RunMessage CosmicEmissaryTheBrilliance where
  runMessage msg e@(CosmicEmissaryTheBrilliance attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows _ -> do
      handleCosmicEmissaryColour attrs windows
      pure e
    _ -> CosmicEmissaryTheBrilliance <$> liftRunMessage msg attrs
