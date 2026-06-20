module Arkham.Enemy.Cards.CosmicEmissaryTheAbyss (cosmicEmissaryTheAbyss) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Scenarios.FateOfTheVale.CosmicEmissary

newtype CosmicEmissaryTheAbyss = CosmicEmissaryTheAbyss EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheAbyss :: EnemyCard CosmicEmissaryTheAbyss
cosmicEmissaryTheAbyss =
  enemyWith CosmicEmissaryTheAbyss Cards.cosmicEmissaryTheAbyss
    $ (healthL .~ Nothing)
    . (asSelfLocationL ?~ "cosmicEmissaryAbyss")

instance HasModifiersFor CosmicEmissaryTheAbyss where
  getModifiersFor (CosmicEmissaryTheAbyss attrs) = modifySelf attrs [CannotMakeAttacksOfOpportunity, CannotBeDamaged, DoNotExhaustEvaded]

instance HasAbilities CosmicEmissaryTheAbyss where
  getAbilities (CosmicEmissaryTheAbyss attrs) = cosmicEmissaryColourAbilities attrs

instance RunMessage CosmicEmissaryTheAbyss where
  runMessage msg e@(CosmicEmissaryTheAbyss attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows _ -> do
      handleCosmicEmissaryColour attrs windows
      pure e
    _ -> CosmicEmissaryTheAbyss <$> liftRunMessage msg attrs
