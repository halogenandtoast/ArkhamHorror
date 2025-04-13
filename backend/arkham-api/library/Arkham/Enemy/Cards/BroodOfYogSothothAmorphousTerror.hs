module Arkham.Enemy.Cards.BroodOfYogSothothAmorphousTerror (broodOfYogSothothAmorphousTerror) where

import Arkham.Asset.Cards.TheDunwichLegacy qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)

newtype BroodOfYogSothothAmorphousTerror = BroodOfYogSothothAmorphousTerror EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor BroodOfYogSothothAmorphousTerror where
  getModifiersFor (BroodOfYogSothothAmorphousTerror a) = do
    healthModifier <- perPlayer 1
    modifySelf
      a
      [ HealthModifier healthModifier
      , CanOnlyBeAttackedByAbilityOn $ singleton Assets.esotericFormula.cardCode
      ]

broodOfYogSothothAmorphousTerror :: EnemyCard BroodOfYogSothothAmorphousTerror
broodOfYogSothothAmorphousTerror =
  enemy
    BroodOfYogSothothAmorphousTerror
    Cards.broodOfYogSothothAmorphousTerror
    (0, Static 1, 0)
    (0, 0)

instance RunMessage BroodOfYogSothothAmorphousTerror where
  runMessage msg (BroodOfYogSothothAmorphousTerror attrs) = runQueueT $ case msg of
    _ -> BroodOfYogSothothAmorphousTerror <$> liftRunMessage msg attrs
