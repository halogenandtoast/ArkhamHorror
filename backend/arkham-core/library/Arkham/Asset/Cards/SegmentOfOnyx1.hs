module Arkham.Asset.Cards.SegmentOfOnyx1 (
  segmentOfOnyx1,
  SegmentOfOnyx1 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator (searchBonded)
import Arkham.Matcher hiding (AssetCard)

newtype SegmentOfOnyx1 = SegmentOfOnyx1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

segmentOfOnyx1 :: AssetCard SegmentOfOnyx1
segmentOfOnyx1 = asset SegmentOfOnyx1 Cards.segmentOfOnyx1

instance HasAbilities SegmentOfOnyx1 where
  getAbilities (SegmentOfOnyx1 attrs) =
    [ controlledAbility
        attrs
        1
        ( AssetCount 3 (AssetControlledBy You <> assetIs Cards.segmentOfOnyx1)
            <> Negate (exists $ assetIs Cards.pendantOfTheQueen) -- unique so we can't have more than one
        )
        $ FastAbility Free
    ]

instance RunMessage SegmentOfOnyx1 where
  runMessage msg a@(SegmentOfOnyx1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mPendant <- listToMaybe <$> searchBonded iid Cards.pendantOfTheQueen
      segments <- selectFields AssetCard $ assetIs Cards.segmentOfOnyx1 <> assetControlledBy iid

      for_ mPendant $ \pendant -> do
        push $ PutCardIntoPlay iid pendant Nothing []

      pushAll $ map (PlaceInBonded iid) segments

      pure a
    _ -> SegmentOfOnyx1 <$> runMessage msg attrs
