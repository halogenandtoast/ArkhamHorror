module Arkham.Location.Cards.TheGallowsSpectral_169 (
  theGallowsSpectral_169,
  TheGallowsSpectral_169 (..),
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
import Arkham.Trait (Trait (Witch), toTraits)

newtype TheGallowsSpectral_169 = TheGallowsSpectral_169 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGallowsSpectral_169 :: LocationCard TheGallowsSpectral_169
theGallowsSpectral_169 = location TheGallowsSpectral_169 Cards.theGallowsSpectral_169 3 (Static 0)

instance HasModifiersFor TheGallowsSpectral_169 where
  getModifiersFor target (TheGallowsSpectral_169 a) | isTarget a target = do
    witchCount <- selectCount $ EnemyWithTrait Witch
    pure $ toModifiers a [ShroudModifier witchCount | witchCount > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities TheGallowsSpectral_169 where
  getAbilities (TheGallowsSpectral_169 a) =
    withRevealedAbilities
      a
      [ haunted
          "Discard the top 3 cards of the standard encounter deck. If a Witch enemy is discarded by this effect, draw it."
          a
          1
      ]

instance RunMessage TheGallowsSpectral_169 where
  runMessage msg l@(TheGallowsSpectral_169 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      regular <- genCard Locations.theGallows_169
      push $ ReplaceLocation (toId attrs) regular Swap
      pure l
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      -- We normally want to avoid this, but we need to target the correct deck
      cards <- scenarioFieldMap ScenarioEncounterDeck (take 3 . unDeck)
      let (witches, rest) = partition (member Witch . toTraits) cards
      pushAll $ map AddToEncounterDiscard rest <> map (InvestigatorDrewEncounterCard iid) witches
      pure l
    _ -> TheGallowsSpectral_169 <$> runMessage msg attrs
