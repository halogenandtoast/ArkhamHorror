module Arkham.Asset.Assets.SegmentOfOnyx1 (segmentOfOnyx1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator (searchBonded)
import Arkham.Matcher hiding (AssetCard)

newtype SegmentOfOnyx1 = SegmentOfOnyx1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

segmentOfOnyx1 :: AssetCard SegmentOfOnyx1
segmentOfOnyx1 = asset SegmentOfOnyx1 Cards.segmentOfOnyx1

instance HasAbilities SegmentOfOnyx1 where
  getAbilities (SegmentOfOnyx1 a) = [controlled a 1 criteria $ FastAbility Free]
   where
    criteria =
      AssetCount 3 (AssetControlledBy You <> assetIs Cards.segmentOfOnyx1)
        <> notExists (assetIs Cards.pendantOfTheQueen) -- unique so we can't have more than one

instance RunMessage SegmentOfOnyx1 where
  runMessage msg a@(SegmentOfOnyx1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      segments <- selectFields AssetCard $ assetIs Cards.segmentOfOnyx1 <> assetControlledBy iid
      for_ segments (placeInBonded iid)

      searchBonded iid Cards.pendantOfTheQueen >>= traverse_ (putCardIntoPlay iid)
      pure a
    _ -> SegmentOfOnyx1 <$> liftRunMessage msg attrs
