module Arkham.Location.Cards.ChapelCryptSpectral_174 (
  chapelCryptSpectral_174,
  ChapelCryptSpectral_174 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Deck
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Hex))

newtype ChapelCryptSpectral_174 = ChapelCryptSpectral_174 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

chapelCryptSpectral_174 :: LocationCard ChapelCryptSpectral_174
chapelCryptSpectral_174 = location ChapelCryptSpectral_174 Cards.chapelCryptSpectral_174 6 (Static 0)

instance HasModifiersFor ChapelCryptSpectral_174 where
  getModifiersFor target (ChapelCryptSpectral_174 attrs) | isTarget attrs target = do
    shouldModifyShroud <-
      selectNone $ investigatorAt (toId attrs) <> HasMatchingTreachery (TreacheryWithTrait Hex)
    pure $ toModifiers attrs [ShroudModifier (-3) | shouldModifyShroud]
  getModifiersFor _ _ = pure []

instance HasAbilities ChapelCryptSpectral_174 where
  getAbilities (ChapelCryptSpectral_174 attrs) =
    withRevealedAbilities
      attrs
      [ haunted
          "Find the topmost Hex treachery in the standard encounter discard pile and put it into play in your threat area."
          attrs
          1
      ]

instance RunMessage ChapelCryptSpectral_174 where
  runMessage msg l@(ChapelCryptSpectral_174 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.chapelCryptSpectral_174
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      discards <- scenarioField ScenarioDiscard
      let mTopHex = find (`cardMatch` CardWithTrait Hex) discards
      for_ mTopHex $ \topHex -> do
        push $ DrewTreachery iid (Just EncounterDiscard) (EncounterCard topHex)
      pure l
    _ -> ChapelCryptSpectral_174 <$> runMessage msg attrs
