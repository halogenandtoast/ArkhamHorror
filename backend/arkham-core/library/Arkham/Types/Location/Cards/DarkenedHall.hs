module Arkham.Types.Location.Cards.DarkenedHall
  ( darkenedHall
  , DarkenedHall(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Timing qualified as Timing

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
  (revealedConnectedMatchersL
  .~ map LocationWithSymbol [Triangle, T, Hourglass, Plus, Squiggle]
  )

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

instance LocationRunner env => RunMessage env DarkenedHall where
  runMessage msg (DarkenedHall attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
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
