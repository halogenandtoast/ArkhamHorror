module Arkham.Location.Cards.LairOfHydra (lairOfHydra, LairOfHydra (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (AncientOne, Sanctum))

newtype LairOfHydra = LairOfHydra LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lairOfHydra :: LocationCard LairOfHydra
lairOfHydra = locationWith LairOfHydra Cards.lairOfHydra 6 (PerPlayer 3) connectsToAdjacent

instance HasModifiersFor LairOfHydra where
  getModifiersFor (LairOfHydra a) = modifySelfMaybe a do
    n <- selectCount $ LocationWithAnyKeys <> withTrait Sanctum
    guard $ n > 0
    pure [ShroudModifier (-n)]

instance HasAbilities LairOfHydra where
  getAbilities (LairOfHydra a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> exists
              ( EnemyWithTrait AncientOne
                  <> mapOneOf enemyIs [Enemies.dagonAwakenedAndEnragedIntoTheMaelstrom, Enemies.hydraAwakenedAndEnraged]
              )
        )
      $ actionAbilityWithCost (OrCost $ map (PlaceKeyCost (toTarget a)) [BlackKey, PurpleKey, WhiteKey])

instance RunMessage LairOfHydra where
  runMessage msg l@(LairOfHydra attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select
          $ mapOneOf enemyIs [Enemies.dagonAwakenedAndEnragedIntoTheMaelstrom, Enemies.hydraAwakenedAndEnraged]
      chooseTargetM iid enemies $ nonAttackEnemyDamage (attrs.ability 1) 3
      pure l
    _ -> LairOfHydra <$> liftRunMessage msg attrs
