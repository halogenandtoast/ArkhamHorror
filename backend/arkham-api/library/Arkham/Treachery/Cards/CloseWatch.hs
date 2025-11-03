module Arkham.Treachery.Cards.CloseWatch (closeWatch) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers (raiseAlarmLevel)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CloseWatch = CloseWatch TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeWatch :: TreacheryCard CloseWatch
closeWatch = treachery CloseWatch Cards.closeWatch

instance RunMessage CloseWatch where
  runMessage msg t@(CloseWatch attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      anyAssets <- selectAny $ AssetWithHighestPrintedCost $ assetControlledBy iid <> DiscardableAsset
      chooseOrRunOneM iid do
        when anyAssets do
          labeled "Discard the asset you control with the highest printed cost" do
            chooseAndDiscardAssetMatching iid attrs $ AssetWithHighestPrintedCost AnyAsset
          labeled "Raise your alarm level by 1" $ raiseAlarmLevel attrs [iid]
      pure t
    _ -> CloseWatch <$> liftRunMessage msg attrs
