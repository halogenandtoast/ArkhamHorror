module Arkham.Location.Cards.TheGallowsSpectral_170 (theGallowsSpectral_170) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Trait (Trait (Geist), toTraits)

newtype TheGallowsSpectral_170 = TheGallowsSpectral_170 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGallowsSpectral_170 :: LocationCard TheGallowsSpectral_170
theGallowsSpectral_170 = location TheGallowsSpectral_170 Cards.theGallowsSpectral_170 0 (Static 0)

instance HasModifiersFor TheGallowsSpectral_170 where
  getModifiersFor (TheGallowsSpectral_170 a) = whenRevealed a do
    geistCount <- selectCount $ EnemyWithTrait Geist
    modifySelf a [ShroudModifier geistCount | geistCount > 0]

instance HasAbilities TheGallowsSpectral_170 where
  getAbilities (TheGallowsSpectral_170 a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "theGallowsSpectral_170.haunted" a 1

instance RunMessage TheGallowsSpectral_170 where
  runMessage msg l@(TheGallowsSpectral_170 attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.theGallows_170
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- We normally want to avoid this, but we need to target the correct deck
      cards <- take 3 . unDeck <$> getSpectralDeck
      let (geists, rest) = partition (member Geist . toTraits) cards
      addToEncounterDiscard rest
      for_ geists \geist -> drawCardFrom iid geist (Deck.EncounterDeckByKey SpectralEncounterDeck)
      pure l
    _ -> TheGallowsSpectral_170 <$> liftRunMessage msg attrs
