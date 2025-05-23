module Arkham.Location.Cards.MainPath (mainPath) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (mainPath)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheDevourerBelow.Helpers
import Arkham.Trait

newtype MainPath = MainPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "mainPath.resign" $ locationResignAction a

instance RunMessage MainPath where
  runMessage msg (MainPath attrs) = MainPath <$> runMessage msg attrs
