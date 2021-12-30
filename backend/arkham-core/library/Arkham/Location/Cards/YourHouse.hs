module Arkham.Location.Cards.YourHouse
  ( YourHouse(..)
  , yourHouse
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype YourHouse = YourHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yourHouse :: LocationCard YourHouse
yourHouse =
  location YourHouse Cards.yourHouse 2 (PerPlayer 1) Squiggle [Circle]

instance HasAbilities YourHouse where
  getAbilities (YourHouse x) | locationRevealed x =
    withBaseAbilities x $
      [ mkAbility x 1
        $ ForcedAbility
            (EnemySpawns Timing.When Anywhere $ enemyIs Cards.ghoulPriest)
      , restrictedAbility x 2 Here (ActionAbility Nothing $ ActionCost 1)
      & abilityLimitL
      .~ PlayerLimit PerTurn 1
      ]
  getAbilities (YourHouse x) = getAbilities x

instance LocationRunner env => RunMessage env YourHouse where
  runMessage msg l@(YourHouse attrs@LocationAttrs {..}) = case msg of
    UseCardAbility _ source [Window _ (Window.EnemySpawns eid _)] 1 _
      | isSource attrs source -> do
        let
          isSpawnMsg = \case
            EnemySpawn _ _ eid' -> eid == eid'
            After (EnemySpawn _ _ eid') -> eid == eid'
            RunWindow _ xs -> flip any xs $ \case
              Window _ (Window.EnemySpawns eid' _) -> eid' == eid
              _ -> False
            _ -> False
        withQueue_ $ filter (not . isSpawnMsg)
        l <$ pushAll
          [ EnemySpawn Nothing locationId eid
          , After (EnemySpawn Nothing locationId eid)
          ]
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      l <$ pushAll [DrawCards iid 1 False, TakeResources iid 1 False]
    _ -> YourHouse <$> runMessage msg attrs
