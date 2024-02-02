module Arkham.Location.Cards.HoleInTheWall where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (holeInTheWall)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype HoleInTheWall = HoleInTheWall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

holeInTheWall :: LocationCard HoleInTheWall
holeInTheWall = location HoleInTheWall Cards.holeInTheWall 1 (Static 0)

instance HasAbilities HoleInTheWall where
  getAbilities (HoleInTheWall attrs) =
    withRevealedAbilities attrs
      $ [ mkAbility attrs 1
            $ ForcedAbility
            $ RevealLocation #after You
            $ LocationWithId
            $ toId attrs
        ]

instance RunMessage HoleInTheWall where
  runMessage msg l@(HoleInTheWall attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      noAttic <- selectNone $ LocationWithTitle "Attic"
      noCellar <- selectNone $ LocationWithTitle "Cellar"
      pushAll
        $ [PlaceLocationMatching "Attic" | noAttic]
        <> [PlaceLocationMatching "Cellar" | noCellar]
        <> [PlaceLocationMatching "Parlor"]
      pure l
    _ -> HoleInTheWall <$> runMessage msg attrs
