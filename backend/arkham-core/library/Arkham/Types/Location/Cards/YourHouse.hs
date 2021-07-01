module Arkham.Types.Location.Cards.YourHouse
  ( YourHouse(..)
  , yourHouse
  )
where

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
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Window

newtype YourHouse = YourHouse LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yourHouse :: LocationId -> YourHouse
yourHouse = YourHouse . baseAttrs
  Cards.yourHouse
  2
  (PerPlayer 1)
  Squiggle
  [Circle]

instance HasModifiersFor env YourHouse where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerTurn 1
    }

instance ActionRunner env => HasActions env YourHouse where
  getActions iid NonFast (YourHouse attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `member` locationInvestigators
      ]
  getActions iid window (YourHouse attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env YourHouse where
  runMessage msg l@(YourHouse attrs@LocationAttrs {..}) = case msg of
    Will spawnMsg@(EnemySpawn miid _ eid) -> do
      cardCode <- getId @CardCode eid
      when (cardCode == "01116") $ do
        withQueue_
          $ filter (and . sequence [(/= After spawnMsg), (/= spawnMsg)])
        unshiftMessages
          [ EnemySpawn miid locationId eid
          , After (EnemySpawn miid locationId eid)
          ]
      YourHouse <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessages [DrawCards iid 1 False, TakeResources iid 1 False]
    _ -> YourHouse <$> runMessage msg attrs
