module Arkham.Location.Cards.MainPath (
  MainPath (..),
  mainPath,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (mainPath)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype MainPath = MainPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

mainPath :: LocationCard MainPath
mainPath =
  locationWith
    MainPath
    Cards.mainPath
    2
    (Static 0)
    (revealedConnectedMatchersL <>~ [LocationWithTrait Woods])

instance HasAbilities MainPath where
  getAbilities (MainPath a) =
    withRevealedAbilities a
      $ [ withTooltip
            "\"There's nothing we can do to stop them!\" You flee from the woods, leaving Arkham to its grisly fate."
            $ locationResignAction a
        ]

instance RunMessage MainPath where
  runMessage msg (MainPath attrs) = MainPath <$> runMessage msg attrs
