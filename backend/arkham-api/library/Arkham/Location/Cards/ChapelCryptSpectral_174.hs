module Arkham.Location.Cards.ChapelCryptSpectral_174 (
  chapelCryptSpectral_174,
  ChapelCryptSpectral_174 (..),
)
where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Hex))

newtype ChapelCryptSpectral_174 = ChapelCryptSpectral_174 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelCryptSpectral_174 :: LocationCard ChapelCryptSpectral_174
chapelCryptSpectral_174 = location ChapelCryptSpectral_174 Cards.chapelCryptSpectral_174 6 (Static 0)

instance HasModifiersFor ChapelCryptSpectral_174 where
  getModifiersFor (ChapelCryptSpectral_174 a) = maybeModifySelf a do
    liftGuardM $ selectNone $ investigatorAt a <> HasMatchingTreachery (TreacheryWithTrait Hex)
    pure [ShroudModifier (-3)]

instance HasAbilities ChapelCryptSpectral_174 where
  getAbilities (ChapelCryptSpectral_174 attrs) =
    extendRevealed1 attrs
      $ haunted
        "Find the topmost Hex treachery in the standard encounter discard pile and put it into play in your threat area."
        attrs
        1

instance RunMessage ChapelCryptSpectral_174 where
  runMessage msg l@(ChapelCryptSpectral_174 attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      spectral <- genCard Locations.chapelCryptSpectral_174
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mTopHex <- find (`cardMatch` CardWithTrait Hex) <$> scenarioField ScenarioDiscard
      for_ mTopHex \topHex -> do
        push $ DrewTreachery iid (Just EncounterDiscard) (EncounterCard topHex)
      pure l
    _ -> ChapelCryptSpectral_174 <$> liftRunMessage msg attrs
