module Arkham.Types.Location.Cards.YourHouse
  ( YourHouse(..)
  , yourHouse
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype YourHouse = YourHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yourHouse :: LocationCard YourHouse
yourHouse =
  location YourHouse Cards.yourHouse 2 (PerPlayer 1) Squiggle [Circle]

instance HasAbilities env YourHouse where
  getAbilities iid window (YourHouse x) | locationRevealed x =
    withBaseAbilities iid window x $ pure
      [ mkAbility x 1
        $ ForcedAbility
            (EnemySpawns Timing.When Anywhere $ enemyIs Cards.ghoulPriest)
      , restrictedAbility x 2 Here (ActionAbility Nothing $ ActionCost 1)
      & abilityLimitL
      .~ PlayerLimit PerTurn 1
      ]
  getAbilities iid window (YourHouse x) = getAbilities iid window x

instance LocationRunner env => RunMessage env YourHouse where
  runMessage msg l@(YourHouse attrs@LocationAttrs {..}) = case msg of
    UseCardAbility _ source [Window _ (Window.EnemySpawns eid _)] 1 _
      | isSource attrs source -> do
        let
          isSpawnMsg = \case
            EnemySpawn _ _ eid' -> eid == eid'
            After (EnemySpawn _ _ eid') -> eid == eid'
            RunWindow _ windows -> flip
              any
              windows
              \case
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
