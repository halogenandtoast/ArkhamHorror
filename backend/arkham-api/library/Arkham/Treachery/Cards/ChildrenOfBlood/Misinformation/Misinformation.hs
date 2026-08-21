module Arkham.Treachery.Cards.ChildrenOfBlood.Misinformation.Misinformation (misinformation) where

import Arkham.Discard
import Arkham.Helpers.Investigator (canPlaceCluesOnYourLocation)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Misinformation qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Misinformation = Misinformation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

misinformation :: TreacheryCard Misinformation
misinformation = treachery Misinformation Cards.misinformation

instance RunMessage Misinformation where
  runMessage msg t@(Misinformation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n (FailedThisSkillTestBy iid (isSource attrs -> True) _) | n > 0 -> do
      canPlaceClues <- canPlaceCluesOnYourLocation iid
      canDiscard <- selectAny $ inHandOf NotForPlay iid <> basic DiscardableCard
      when (canPlaceClues || canDiscard) do
        chooseOneM iid $ withI18n do
          countVar 1
            $ labeledValidate' canPlaceClues "placeCluesOnYourLocation"
            $ placeCluesOnLocation iid attrs 1
          countVar 1
            $ labeledValidate' canDiscard "discardCardsFromHand"
            $ discardFromHand iid attrs DiscardChoose 1
        doNextStep msg
      pure t
    _ -> Misinformation <$> liftRunMessage msg attrs
