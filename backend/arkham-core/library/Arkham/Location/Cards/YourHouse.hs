module Arkham.Location.Cards.YourHouse (
  YourHouse (..),
  yourHouse,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype YourHouse = YourHouse LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yourHouse :: LocationCard YourHouse
yourHouse = location YourHouse Cards.yourHouse 2 (PerPlayer 1)

instance HasModifiersFor YourHouse where
  getModifiersFor (EnemyTarget eid) (YourHouse attrs) = do
    isGhoulPriest <- eid <=~> enemyIs Cards.ghoulPriest
    pure $ toModifiers attrs [ForceSpawnLocation (LocationWithId $ toId attrs) | isGhoulPriest]
  getModifiersFor _ _ = pure []

-- Ability 1 is just to trigger in UI, it doesn't change anything since we
-- handle it with the modifier
instance HasAbilities YourHouse where
  getAbilities (YourHouse x) =
    withRevealedAbilities x
      $ [ forcedAbility x 1 $ EnemySpawns Timing.When Anywhere $ enemyIs Cards.ghoulPriest
        , limitedAbility (PlayerLimit PerTurn 1)
            $ restrictedAbility x 2 Here (ActionAbility Nothing $ ActionCost 1)
        ]

instance RunMessage YourHouse where
  runMessage msg l@(YourHouse attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      drawing <- drawCards iid (toAbilitySource attrs 2) 1
      pushAll [drawing, TakeResources iid 1 (toAbilitySource attrs 2) False]
      pure l
    _ -> YourHouse <$> runMessage msg attrs
