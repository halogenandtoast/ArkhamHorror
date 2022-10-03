module Arkham.Location.Cards.DarkenedHall
  ( darkenedHall
  , DarkenedHall(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Timing qualified as Timing

newtype DarkenedHall = DarkenedHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkenedHall :: LocationCard DarkenedHall
darkenedHall = location DarkenedHall Cards.darkenedHall 4 (Static 0)

instance HasAbilities DarkenedHall where
  getAbilities (DarkenedHall x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ RevealLocation Timing.After Anyone
      $ LocationWithId
      $ toId x
    | locationRevealed x
    ]

instance RunMessage DarkenedHall where
  runMessage msg (DarkenedHall attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      artGallery <- getSetAsideCard Cards.artGallery
      vipArea <- getSetAsideCard Cards.vipArea
      backAlley <- getSetAsideCard Cards.backAlley

      locationsWithLabels <-
        zip ["backHallDoorway1", "backHallDoorway2", "backHallDoorway3"]
          <$> shuffleM [artGallery, vipArea, backAlley]

      pushAll $ concat
        [ [ PlaceLocation location'
          , SetLocationLabel (LocationId $ toCardId location') label'
          ]
        | (label', location') <- locationsWithLabels
        ]
      DarkenedHall <$> runMessage msg attrs
    _ -> DarkenedHall <$> runMessage msg attrs
