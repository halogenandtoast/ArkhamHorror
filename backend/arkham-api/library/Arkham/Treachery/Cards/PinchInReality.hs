module Arkham.Treachery.Cards.PinchInReality (pinchInReality) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Helpers.Investigator
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PinchInReality = PinchInReality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pinchInReality :: TreacheryCard PinchInReality
pinchInReality = treachery PinchInReality Cards.pinchInReality

instance RunMessage PinchInReality where
  runMessage msg t@(PinchInReality attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      canSpend <- getCanSpendNClues iid 1
      n <- selectCount $ inHandOf NotForPlay iid <> basic NonWeakness
      chooseOrRunOneM iid do
        withI18n $ countVar 1 $ labeledValidate' canSpend "spendClues" $ spendClues iid 1
        campaignI18n $ countVar 2 $ labeledValidate' (n > 0) "pinchInReality.discard" $ doStep 1 msg
        when (not canSpend && n == 0) $ withI18n $ labeled' "continue" nothing
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      n <- selectCount $ inHandOf NotForPlay iid <> basic NonWeakness
      chooseAndDiscardCards iid attrs ((n + 1) `div` 2)
      pure t
    _ -> PinchInReality <$> liftRunMessage msg attrs
