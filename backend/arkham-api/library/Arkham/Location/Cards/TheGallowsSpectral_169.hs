module Arkham.Location.Cards.TheGallowsSpectral_169 (theGallowsSpectral_169) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Trait (Trait (Witch), toTraits)

newtype TheGallowsSpectral_169 = TheGallowsSpectral_169 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGallowsSpectral_169 :: LocationCard TheGallowsSpectral_169
theGallowsSpectral_169 = location TheGallowsSpectral_169 Cards.theGallowsSpectral_169 3 (Static 0)

instance HasModifiersFor TheGallowsSpectral_169 where
  getModifiersFor (TheGallowsSpectral_169 a) = whenRevealed a do
    witchCount <- selectCount $ EnemyWithTrait Witch
    modifySelf a [ShroudModifier witchCount]

instance HasAbilities TheGallowsSpectral_169 where
  getAbilities (TheGallowsSpectral_169 a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "theGallowsSpectral_169.haunted" a 1

instance RunMessage TheGallowsSpectral_169 where
  runMessage msg l@(TheGallowsSpectral_169 attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.theGallows_169
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- We normally want to avoid this, but we need to target the correct deck
      cards <- scenarioFieldMap ScenarioEncounterDeck (take 3 . unDeck)
      let (witches, rest) = partition (member Witch . toTraits) cards
      addToEncounterDiscard rest
      for_ witches \witch -> drawCardFrom iid witch Deck.EncounterDeck
      pure l
    _ -> TheGallowsSpectral_169 <$> liftRunMessage msg attrs
