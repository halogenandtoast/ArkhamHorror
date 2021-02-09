module Arkham.Types.Location.Cards.DarkenedHall
  ( darkenedHall
  , DarkenedHall(..)
  )
where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait hiding (Cultist)

newtype DarkenedHall = DarkenedHall LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkenedHall :: DarkenedHall
darkenedHall = DarkenedHall $ base
  { locationRevealedConnectedSymbols = setFromList
    [Triangle, T, Hourglass, Plus, Squiggle]
  }
 where
  base = baseAttrs
    "02074"
    (Name "Darkened Hall" Nothing)
    EncounterSet.TheHouseAlwaysWins
    4
    (Static 0)
    Diamond
    [Triangle]
    [CloverClub]

instance HasModifiersFor env DarkenedHall where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env DarkenedHall where
  getActions iid window (DarkenedHall attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env DarkenedHall where
  runMessage msg (DarkenedHall attrs@LocationAttrs {..}) = case msg of
    RevealLocation _ lid | lid == locationId -> do
      locations <- shuffleM ["02075", "02076", "02077"]
      unshiftMessages $ concat
        [ [PlaceLocation location, SetLocationLabel location label']
        | (label', location) <- zip
          ["backHallDoorway1", "backHallDoorway2", "backHallDoorway3"]
          locations
        ]
      DarkenedHall <$> runMessage msg attrs
    _ -> DarkenedHall <$> runMessage msg attrs
