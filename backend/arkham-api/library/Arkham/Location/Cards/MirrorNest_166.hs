module Arkham.Location.Cards.MirrorNest_166 (mirrorNest_166) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype MirrorNest_166 = MirrorNest_166 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirrorNest_166 :: LocationCard MirrorNest_166
mirrorNest_166 = location MirrorNest_166 Cards.mirrorNest_166 3 (PerPlayer 2)

instance HasAbilities MirrorNest_166 where
  getAbilities (MirrorNest_166 a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced
      $ TurnEnds #when (You <> not_ SuccessfullyAttackedThisRound)

instance RunMessage MirrorNest_166 where
  runMessage msg l@(MirrorNest_166 attrs) = runQueueT $ case msg of
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
    _ -> MirrorNest_166 <$> liftRunMessage msg attrs
