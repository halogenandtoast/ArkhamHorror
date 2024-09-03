module Arkham.Location.Cards.ChapelAtticSpectral_176 (
  chapelAtticSpectral_176,
  ChapelAtticSpectral_176 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype ChapelAtticSpectral_176 = ChapelAtticSpectral_176 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelAtticSpectral_176 :: LocationCard ChapelAtticSpectral_176
chapelAtticSpectral_176 = location ChapelAtticSpectral_176 Cards.chapelAtticSpectral_176 8 (Static 0)

instance HasModifiersFor ChapelAtticSpectral_176 where
  getModifiersFor (InvestigatorTarget iid) (ChapelAtticSpectral_176 a) = do
    investigating <- isInvestigating iid (toId a)
    cardCount <- fieldMap InvestigatorHand length iid
    pure $ toModifiers a [AnySkillValue cardCount | investigating]
  getModifiersFor _ _ = pure []

instance HasAbilities ChapelAtticSpectral_176 where
  getAbilities (ChapelAtticSpectral_176 attrs) =
    withRevealedAbilities
      attrs
      [ withTooltip
          " Discard a random card from your hand (2 cards instead of you have 5 or more cards in hand)."
          $ restrictedAbility attrs 1 (InvestigatorExists $ You <> HandWith AnyCards) Haunted
      ]

instance RunMessage ChapelAtticSpectral_176 where
  runMessage msg l@(ChapelAtticSpectral_176 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.chapelAtticSpectral_176
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      handCount <- fieldMap InvestigatorHand length iid
      push $ toMessage $ randomDiscardN iid attrs (if handCount >= 5 then 2 else 1)
      pure l
    _ -> ChapelAtticSpectral_176 <$> runMessage msg attrs
