module Arkham.Types.Location.Cards.DarkenedHall
  ( darkenedHall
  , DarkenedHall(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import qualified Arkham.Types.Timing as Timing

newtype DarkenedHall = DarkenedHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
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

instance HasAbilities env DarkenedHall where
  getAbilities i w (DarkenedHall x) = withBaseAbilities i w x $ pure
    [ mkAbility x 1
      $ ForcedAbility
      $ RevealLocation Timing.After Anyone
      $ LocationWithId
      $ toId x
    | locationRevealed x
    ]

instance LocationRunner env => RunMessage env DarkenedHall where
  runMessage msg (DarkenedHall attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
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
