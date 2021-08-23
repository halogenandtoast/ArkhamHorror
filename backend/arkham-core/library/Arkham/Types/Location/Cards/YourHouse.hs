module Arkham.Types.Location.Cards.YourHouse
  ( YourHouse(..)
  , yourHouse
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (yourHouse)
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype YourHouse = YourHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yourHouse :: LocationCard YourHouse
yourHouse =
  location YourHouse Cards.yourHouse 2 (PerPlayer 1) Squiggle [Circle]

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerTurn 1
    }

instance HasAbilities env YourHouse where
  getAbilities iid window@(Window Timing.When NonFast) (YourHouse attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseAbilities iid window attrs $ pure [locationAbility (ability attrs)]
  getAbilities iid window (YourHouse attrs) = getAbilities iid window attrs

instance (LocationRunner env) => RunMessage env YourHouse where
  runMessage msg l@(YourHouse attrs@LocationAttrs {..}) = case msg of
    Will spawnMsg@(EnemySpawn miid _ eid) -> do
      cardCode <- getId @CardCode eid
      when (cardCode == "01116") $ do
        withQueue_
          $ filter (and . sequence [(/= After spawnMsg), (/= spawnMsg)])
        pushAll
          [ EnemySpawn miid locationId eid
          , After (EnemySpawn miid locationId eid)
          ]
      YourHouse <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ pushAll [DrawCards iid 1 False, TakeResources iid 1 False]
    _ -> YourHouse <$> runMessage msg attrs
