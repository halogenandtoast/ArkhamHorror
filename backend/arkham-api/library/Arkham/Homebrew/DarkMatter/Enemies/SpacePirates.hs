module Arkham.Homebrew.DarkMatter.Enemies.SpacePirates (spacePirates) where

import Arkham.Ability
import Arkham.Cost.FieldCost (MaybeFieldCost (MaybeFieldCost))
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyRemainingHealth))
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Starship)
import Arkham.Matcher

newtype SpacePirates = SpacePirates EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Spawn - Nearest [[Starship]] location."
spacePirates :: EnemyCard SpacePirates
spacePirates =
  enemy SpacePirates Cards.spacePirates
    & setSpawnAt (NearestLocationToYou $ LocationWithTrait Starship)

{- | "[action] Spend resources equal to Space Pirates' remaining health: Parley.
Discard Space Pirates."
-}
instance HasAbilities SpacePirates where
  getAbilities (SpacePirates a) =
    extend1 a
      $ restricted a 1 OnSameLocation
      $ parleyAction (MaybeFieldResourceCost $ MaybeFieldCost (EnemyWithId a.id) EnemyRemainingHealth)

instance RunMessage SpacePirates where
  runMessage msg e@(SpacePirates attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> SpacePirates <$> liftRunMessage msg attrs
