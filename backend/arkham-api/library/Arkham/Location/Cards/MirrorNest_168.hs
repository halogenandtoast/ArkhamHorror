module Arkham.Location.Cards.MirrorNest_168 (mirrorNest_168) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype MirrorNest_168 = MirrorNest_168 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirrorNest_168 :: LocationCard MirrorNest_168
mirrorNest_168 = location MirrorNest_168 Cards.mirrorNest_168 3 (PerPlayer 2)

instance HasAbilities MirrorNest_168 where
  getAbilities (MirrorNest_168 a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced
      $ TurnEnds #when (You <> not_ SuccessfullyInvestigatedThisRound)

instance RunMessage MirrorNest_168 where
  runMessage msg l@(MirrorNest_168 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clueCount <- field InvestigatorClues iid
      hasCards <- selectAny $ inHandOf NotForPlay iid
      chooseOrRunOneM iid $ withI18n do
        when (clueCount > 0)
          $ countVar 1
          $ labeled' "placeCluesOnYourLocation"
          $ placeCluesOnLocation iid (attrs.ability 1) 1
        when hasCards
          $ countVar 1
          $ labeled' "discardRandomCardsFromHand"
          $ randomDiscardN iid (attrs.ability 1) 1
      pure l
    _ -> MirrorNest_168 <$> liftRunMessage msg attrs
