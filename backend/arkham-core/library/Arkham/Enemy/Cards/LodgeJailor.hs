module Arkham.Enemy.Cards.LodgeJailor (
  lodgeJailor,
  LodgeJailor (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.ForTheGreaterGood.Helpers
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Sanctum))

newtype LodgeJailor = LodgeJailor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeJailor :: EnemyCard LodgeJailor
lodgeJailor =
  enemyWith
    LodgeJailor
    Cards.lodgeJailor
    (2, Static 3, 3)
    (0, 2)
    (spawnAtL ?~ SpawnLocation (LocationWithTrait Sanctum))

instance HasAbilities LodgeJailor where
  getAbilities (LodgeJailor attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns Timing.After Anywhere $ EnemyWithId $ toId attrs
      ]

instance RunMessage LodgeJailor where
  runMessage msg e@(LodgeJailor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      mKey <- getRandomKey
      pushAll $
        PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 2
          : [PlaceKey (toTarget attrs) k | k <- maybeToList mKey]
      pure e
    _ -> LodgeJailor <$> runMessage msg attrs
