module Arkham.Treachery.Cards.Torment (torment) where

import Arkham.Card
import Arkham.Helpers.SkillTest.Lifted (beginSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (beginSkillTest)

newtype Torment = Torment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torment :: TreacheryCard Torment
torment = treachery Torment Cards.torment

instance RunMessage Torment where
  runMessage msg t@(Torment attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      beginSkillTest sid iid (toSource attrs) iid #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid do
        labeled "Each investigator discards an asset" do
          eachInvestigator \iid' -> do
            hand <- field InvestigatorHand iid'
            let assets = filter (\c -> cdCardType (toCardDef c) == AssetType) hand
            unless (null assets) do
              chooseTargetM iid' assets $ discardCard iid' attrs
        labeled "Each investigator discards an event" do
          eachInvestigator \iid' -> do
            hand <- field InvestigatorHand iid'
            let events = filter (\c -> cdCardType (toCardDef c) == EventType) hand
            unless (null events) do
              chooseTargetM iid' events $ discardCard iid' attrs
        labeled "Each investigator discards a skill" do
          eachInvestigator \iid' -> do
            hand <- field InvestigatorHand iid'
            let skills = filter (\c -> cdCardType (toCardDef c) == SkillType) hand
            unless (null skills) do
              chooseTargetM iid' skills $ discardCard iid' attrs
      pure t
    _ -> Torment <$> liftRunMessage msg attrs
