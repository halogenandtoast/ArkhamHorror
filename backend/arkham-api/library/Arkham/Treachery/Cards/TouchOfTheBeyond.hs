module Arkham.Treachery.Cards.TouchOfTheBeyond (touchOfTheBeyond) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TouchOfTheBeyond = TouchOfTheBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

touchOfTheBeyond :: TreacheryCard TouchOfTheBeyond
touchOfTheBeyond = treachery TouchOfTheBeyond Cards.touchOfTheBeyond

instance RunMessage TouchOfTheBeyond where
  runMessage msg t@(TouchOfTheBeyond attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- select $ AssetWithHighestPrintedCost $ assetControlledBy iid
      if null assets
        then gainSurge attrs
        else chooseTargetM iid assets (placeDoomOn attrs 1)
      pure t
    _ -> TouchOfTheBeyond <$> liftRunMessage msg attrs
