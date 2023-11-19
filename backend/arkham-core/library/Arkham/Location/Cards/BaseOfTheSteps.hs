module Arkham.Location.Cards.BaseOfTheSteps (
  baseOfTheSteps,
  BaseOfTheSteps (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype BaseOfTheSteps = BaseOfTheSteps LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseOfTheSteps :: LocationCard BaseOfTheSteps
baseOfTheSteps = location BaseOfTheSteps Cards.baseOfTheSteps 3 (PerPlayer 1)

instance HasModifiersFor BaseOfTheSteps where
  getModifiersFor target (BaseOfTheSteps a) | a `is` target = do
    blocked <- selectAny $ locationIs Cards.sevenHundredSteps <> LocationWithAnyClues
    pure $ toModifiers a [Blocked | blocked]
  getModifiersFor _ _ = pure []

instance HasAbilities BaseOfTheSteps where
  getAbilities (BaseOfTheSteps attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage BaseOfTheSteps where
  runMessage msg (BaseOfTheSteps attrs) =
    BaseOfTheSteps <$> runMessage msg attrs
