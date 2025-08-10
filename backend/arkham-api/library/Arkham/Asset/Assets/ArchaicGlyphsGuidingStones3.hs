module Arkham.Asset.Assets.ArchaicGlyphsGuidingStones3 (archaicGlyphsGuidingStones3) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest.Lifted (investigateEdit_)
import Arkham.Location.Types (Field (..))
import Arkham.Projection

newtype ArchaicGlyphsGuidingStones3 = ArchaicGlyphsGuidingStones3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities ArchaicGlyphsGuidingStones3 where
  getAbilities (ArchaicGlyphsGuidingStones3 a) =
    [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

archaicGlyphsGuidingStones3 :: AssetCard ArchaicGlyphsGuidingStones3
archaicGlyphsGuidingStones3 = asset ArchaicGlyphsGuidingStones3 Cards.archaicGlyphsGuidingStones3

instance RunMessage ArchaicGlyphsGuidingStones3 where
  runMessage msg a@(ArchaicGlyphsGuidingStones3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigateEdit_ sid iid (attrs.ability 1) (setTarget attrs)
      pure a
    Successful (Action.Investigate, LocationTarget lid) iid _ (isTarget attrs -> True) n -> do
      clueCount <- field LocationClues lid
      let additional = n `div` 2
      discoverAt IsInvestigate iid (attrs.ability 1) (min clueCount (1 + additional)) lid
      pure a
    _ -> ArchaicGlyphsGuidingStones3 <$> liftRunMessage msg attrs
