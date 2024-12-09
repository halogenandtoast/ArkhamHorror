module Arkham.Location.Cards.ChapelAtticSpectral_176 (
  chapelAtticSpectral_176,
  ChapelAtticSpectral_176 (..),
)
where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigating)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Projection

newtype ChapelAtticSpectral_176 = ChapelAtticSpectral_176 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelAtticSpectral_176 :: LocationCard ChapelAtticSpectral_176
chapelAtticSpectral_176 = location ChapelAtticSpectral_176 Cards.chapelAtticSpectral_176 8 (Static 0)

instance HasModifiersFor ChapelAtticSpectral_176 where
  getModifiersFor (ChapelAtticSpectral_176 a) =
    getSkillTestInvestigator >>= \case
      Nothing -> pure mempty
      Just iid -> maybeModified_ a iid do
        liftGuardM $ isInvestigating iid a
        cardCount <- fieldMap InvestigatorHand length iid
        pure [AnySkillValue cardCount | cardCount > 0]

instance HasAbilities ChapelAtticSpectral_176 where
  getAbilities (ChapelAtticSpectral_176 attrs) =
    extendRevealed1 attrs
      $ withTooltip
        " Discard a random card from your hand (2 cards instead of you have 5 or more cards in hand)."
      $ restricted attrs 1 (youExist $ HandWith AnyCards) Haunted

instance RunMessage ChapelAtticSpectral_176 where
  runMessage msg l@(ChapelAtticSpectral_176 attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      spectral <- genCard Locations.chapelAtticSpectral_176
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      handCount <- fieldMap InvestigatorHand length iid
      randomDiscardN iid attrs (if handCount >= 5 then 2 else 1)
      pure l
    _ -> ChapelAtticSpectral_176 <$> liftRunMessage msg attrs
