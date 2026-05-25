module Arkham.Enemy.Cards.ThingInTheDepths (thingInTheDepths) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.GameValue
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf, modifySelfMaybe)
import Arkham.Matcher
import Arkham.Trait (Trait (Bog, Sunken))

newtype ThingInTheDepths = ThingInTheDepths EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thingInTheDepths :: EnemyCard ThingInTheDepths
thingInTheDepths = enemy ThingInTheDepths Cards.thingInTheDepths (4, Static 10, 2) (2, 2)

instance HasModifiersFor ThingInTheDepths where
  getModifiersFor (ThingInTheDepths a) = do
    n <- perPlayer 5
    modifySelf a [CannotMakeAttacksOfOpportunity, HealthModifier n]
    sunkenLocations <- select $ LocationWithTrait Sunken
    modifySelfMaybe a do
      loc <- MaybeT $ getLocationOf a
      guard $ notNull sunkenLocations
      pure [HunterConnectedTo x | x <- sunkenLocations, x /= loc]

instance HasAbilities ThingInTheDepths where
  getAbilities (ThingInTheDepths a) =
    extend1 a
      $ restricted a 1 (exists $ be a <> EnemyAt (LocationWithTrait Bog))
      $ forced
      $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage ThingInTheDepths where
  runMessage msg e@(ThingInTheDepths attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 2
      pure e
    _ -> ThingInTheDepths <$> liftRunMessage msg attrs
