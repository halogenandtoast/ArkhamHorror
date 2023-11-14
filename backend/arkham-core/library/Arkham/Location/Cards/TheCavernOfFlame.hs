module Arkham.Location.Cards.TheCavernOfFlame (
  theCavernOfFlame,
  TheCavernOfFlame (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype TheCavernOfFlame = TheCavernOfFlame LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCavernOfFlame :: LocationCard TheCavernOfFlame
theCavernOfFlame = location TheCavernOfFlame Cards.theCavernOfFlame 9 (Static 0)

instance HasModifiersFor TheCavernOfFlame where
  getModifiersFor target (TheCavernOfFlame a) | a `is` target = do
    blocked <- selectAny $ locationIs Cards.seventySteps <> LocationWithAnyClues
    pure $ toModifiers a [Blocked | blocked]
  getModifiersFor _ _ = pure []

instance HasAbilities TheCavernOfFlame where
  getAbilities (TheCavernOfFlame attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage TheCavernOfFlame where
  runMessage msg (TheCavernOfFlame attrs) =
    TheCavernOfFlame <$> runMessage msg attrs
