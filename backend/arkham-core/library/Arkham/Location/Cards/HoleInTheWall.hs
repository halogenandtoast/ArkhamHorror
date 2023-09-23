module Arkham.Location.Cards.HoleInTheWall where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (holeInTheWall)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype HoleInTheWall = HoleInTheWall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holeInTheWall :: LocationCard HoleInTheWall
holeInTheWall = location HoleInTheWall Cards.holeInTheWall 1 (Static 0)

instance HasAbilities HoleInTheWall where
  getAbilities (HoleInTheWall attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage HoleInTheWall where
  runMessage msg l@(HoleInTheWall attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          l
            <$ pushAll
              [ PlaceLocationMatching (CardWithTitle "Attic")
              , PlaceLocationMatching (CardWithTitle "Cellar")
              , PlaceLocationMatching (CardWithTitle "Parlor")
              ]
    _ -> HoleInTheWall <$> runMessage msg attrs
