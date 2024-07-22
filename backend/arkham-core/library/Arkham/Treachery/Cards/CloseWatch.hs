module Arkham.Treachery.Cards.CloseWatch (closeWatch, CloseWatch (..)) where

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner hiding (chooseOrRunOne)

newtype CloseWatch = CloseWatch TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeWatch :: TreacheryCard CloseWatch
closeWatch = treachery CloseWatch Cards.closeWatch

instance RunMessage CloseWatch where
  runMessage msg t@(CloseWatch attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      anyAssets <- selectAny $ AssetWithHighestPrintedCost $ assetControlledBy iid <> DiscardableAsset
      chooseOrRunOne iid
        $ [ Label
            "Discard the asset you control with the highest printed cost"
            [ChooseAndDiscardAsset iid (toSource attrs) $ AssetWithHighestPrintedCost AnyAsset]
          | anyAssets
          ]
        <> [Label "Raise your alarm level by 1" [PlaceTokens (toSource attrs) (toTarget iid) AlarmLevel 1]]
      pure t
    _ -> CloseWatch <$> liftRunMessage msg attrs
