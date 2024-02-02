module Arkham.Location.Cards.TheEnchantedPath (
  theEnchantedPath,
  TheEnchantedPath (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype TheEnchantedPath = TheEnchantedPath LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theEnchantedPath :: LocationCard TheEnchantedPath
theEnchantedPath =
  locationWith
    TheEnchantedPath
    Cards.theEnchantedPath
    2
    (Static 0)
    (revealedConnectedMatchersL <>~ ["Enchanted Woods"])

instance HasModifiersFor TheEnchantedPath where
  getModifiersFor target (TheEnchantedPath a) | a `is` target = do
    blocked <- selectAny $ locationIs Cards.baseOfTheSteps <> LocationWithAnyClues
    shroud <- selectCount $ "Enchanted Woods" <> UnrevealedLocation
    pure $ toModifiers a $ [Blocked | blocked] <> [ShroudModifier shroud | locationRevealed a]
  getModifiersFor _ _ = pure []

instance HasAbilities TheEnchantedPath where
  getAbilities (TheEnchantedPath attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 Here $ ActionAbility [] (ActionCost 3)]

instance RunMessage TheEnchantedPath where
  runMessage msg l@(TheEnchantedPath attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1
      pure l
    _ -> TheEnchantedPath <$> runMessage msg attrs
