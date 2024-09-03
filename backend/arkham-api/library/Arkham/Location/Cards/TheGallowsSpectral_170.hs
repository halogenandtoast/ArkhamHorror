module Arkham.Location.Cards.TheGallowsSpectral_170 (
  theGallowsSpectral_170,
  TheGallowsSpectral_170 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Geist), toTraits)

newtype TheGallowsSpectral_170 = TheGallowsSpectral_170 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGallowsSpectral_170 :: LocationCard TheGallowsSpectral_170
theGallowsSpectral_170 = location TheGallowsSpectral_170 Cards.theGallowsSpectral_170 0 (Static 0)

instance HasModifiersFor TheGallowsSpectral_170 where
  getModifiersFor target (TheGallowsSpectral_170 attrs) | isTarget attrs target = do
    geistCount <- selectCount $ EnemyWithTrait Geist
    pure $ toModifiers attrs [ShroudModifier geistCount | geistCount > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities TheGallowsSpectral_170 where
  getAbilities (TheGallowsSpectral_170 a) =
    withRevealedAbilities
      a
      [ haunted
          "Discard the top 3 cards of the standard encounter deck. If a Geist enemy is discarded by this effect, draw it."
          a
          1
      ]

instance RunMessage TheGallowsSpectral_170 where
  runMessage msg l@(TheGallowsSpectral_170 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      regular <- genCard Locations.theGallows_170
      push $ ReplaceLocation (toId attrs) regular Swap
      pure l
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      -- We normally want to avoid this, but we need to target the correct deck
      cards <- scenarioFieldMap ScenarioEncounterDeck (take 3 . unDeck)
      let (geists, rest) = partition (member Geist . toTraits) cards
      pushAll $ map AddToEncounterDiscard rest <> map (InvestigatorDrewEncounterCard iid) geists
      pure l
    _ -> TheGallowsSpectral_170 <$> runMessage msg attrs
