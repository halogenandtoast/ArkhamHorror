module Arkham.Enemy.Cards.StalkingHybrid (stalkingHybrid) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.TheTwistedHollow.Helpers

newtype StalkingHybrid = StalkingHybrid EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkingHybrid :: EnemyCard StalkingHybrid
stalkingHybrid =
  enemy StalkingHybrid Cards.stalkingHybrid (3, Static 1, 3) (1, 1)
    & setOnlyPrey (ControlsAsset $ AssetWithTitle "Vale Lantern")

instance HasModifiersFor StalkingHybrid where
  getModifiersFor (StalkingHybrid a) = do
    x <- getDarknessLevel
    modifySelfWhen a (x > 1) [HealthModifier (x - 1)]

instance HasAbilities StalkingHybrid where
  getAbilities (StalkingHybrid a) = extend a [mkAbility a 1 $ forced (EnemyAttacks #after You AnyEnemyAttack $ be a)]

instance RunMessage StalkingHybrid where
  runMessage msg e@(StalkingHybrid attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (assetControlledBy iid <> AssetWithTitle "Vale Lantern") \lantern -> do
        flipOverBy iid (attrs.ability 1) lantern
        withLocationOf iid $ place lantern . AtLocation
      pure e
    _ -> StalkingHybrid <$> liftRunMessage msg attrs
