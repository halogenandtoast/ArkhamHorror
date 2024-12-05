module Arkham.Location.Cards.TheEnchantedPath (theEnchantedPath, TheEnchantedPath (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheEnchantedPath = TheEnchantedPath LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEnchantedPath :: LocationCard TheEnchantedPath
theEnchantedPath =
  locationWith TheEnchantedPath Cards.theEnchantedPath 2 (Static 0)
    $ revealedConnectedMatchersL
    <>~ ["Enchanted Woods"]

instance HasModifiersFor TheEnchantedPath where
  getModifiersFor (TheEnchantedPath a) = do
    blocked <- selectAny $ locationIs Cards.baseOfTheSteps <> LocationWithAnyClues
    shroud <- selectCount $ "Enchanted Woods" <> UnrevealedLocation
    modifySelf a $ [Blocked | blocked] <> [ShroudModifier shroud | a.revealed]

instance HasAbilities TheEnchantedPath where
  getAbilities (TheEnchantedPath a) =
    extendRevealed1 a $ restricted a 1 Here $ ActionAbility [] (ActionCost 3)

instance RunMessage TheEnchantedPath where
  runMessage msg l@(TheEnchantedPath attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs 1
      pure l
    _ -> TheEnchantedPath <$> liftRunMessage msg attrs
