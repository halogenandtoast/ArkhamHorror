module Arkham.Types.Location.Cards.NorthsideTrainStation
  ( NorthsideTrainStation(..)
  , northsideTrainStation
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (northsideTrainStation)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import Arkham.Types.Window

newtype NorthsideTrainStation = NorthsideTrainStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northsideTrainStation :: LocationCard NorthsideTrainStation
northsideTrainStation = location
  NorthsideTrainStation
  Cards.northsideTrainStation
  2
  (PerPlayer 1)
  T
  [Diamond, Triangle]

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance HasAbilities env NorthsideTrainStation where
  getAbilities iid window@(Window Timing.When NonFast) (NorthsideTrainStation attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseActions iid window attrs $ pure [locationAbility (ability attrs)]
  getAbilities iid window (NorthsideTrainStation attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env NorthsideTrainStation where
  runMessage msg l@(NorthsideTrainStation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationIds <- getSetList [Arkham]
      l <$ push
        (chooseOne
          iid
          [ TargetLabel
              (LocationTarget lid)
              [MoveTo iid lid, MovedBy iid (toSource attrs)]
          | lid <- locationIds
          ]
        )
    _ -> NorthsideTrainStation <$> runMessage msg attrs
