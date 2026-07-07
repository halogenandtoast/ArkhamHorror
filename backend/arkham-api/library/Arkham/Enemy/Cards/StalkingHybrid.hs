module Arkham.Enemy.Cards.StalkingHybrid (stalkingHybrid) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.TheTwistedHollow.Helpers

newtype StalkingHybrid = StalkingHybrid EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkingHybrid :: EnemyCard StalkingHybrid
stalkingHybrid =
  enemy StalkingHybrid Cards.stalkingHybrid
    & setOnlyPrey (ControlsAsset "Vale Lantern")

instance HasModifiersFor StalkingHybrid where
  getModifiersFor (StalkingHybrid a) = do
    x <- getDarknessLevel
    modifySelf a [HealthModifier x]

instance HasAbilities StalkingHybrid where
  getAbilities (StalkingHybrid a) =
    extend a
      $ [mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack $ be a]
      <> [ mkAbility a 2 $ silent $ TookControlOfAsset #after Anyone "Vale Lantern"
         | isInPlayPlacement a.placement
         ]

instance RunMessage StalkingHybrid where
  runMessage msg e@(StalkingHybrid attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withMatch (assetControlledBy iid <> "Vale Lantern") \lantern -> do
        flipOverBy iid (attrs.ability 1) lantern
        withLocationOf iid $ place lantern
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      enemyCheckEngagement attrs.id
      pure e
    _ -> StalkingHybrid <$> liftRunMessage msg attrs
