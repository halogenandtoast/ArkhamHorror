module Arkham.Types.Location.Cards.DarkenedHall
  ( darkenedHall
  , DarkenedHall(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype DarkenedHall = DarkenedHall LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkenedHall :: LocationCard DarkenedHall
darkenedHall = locationWith
  DarkenedHall
  Cards.darkenedHall
  4
  (Static 0)
  Diamond
  [Triangle]
  (revealedConnectedSymbolsL
  .~ setFromList [Triangle, T, Hourglass, Plus, Squiggle]
  )

instance HasModifiersFor env DarkenedHall where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env DarkenedHall where
  getActions iid window (DarkenedHall attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env DarkenedHall where
  runMessage msg (DarkenedHall attrs@LocationAttrs {..}) = case msg of
    RevealLocation _ lid | lid == locationId -> do
      locations <- shuffleM [Cards.artGallery, Cards.vipArea, Cards.backAlley]
      randomIds <- replicateM 3 getRandom
      pushAll $ concat
        [ [ PlaceLocation locationId' location'
          , SetLocationLabel locationId' label'
          ]
        | (locationId', (label', location')) <- zip randomIds $ zip
          ["backHallDoorway1", "backHallDoorway2", "backHallDoorway3"]
          locations
        ]
      DarkenedHall <$> runMessage msg attrs
    _ -> DarkenedHall <$> runMessage msg attrs
