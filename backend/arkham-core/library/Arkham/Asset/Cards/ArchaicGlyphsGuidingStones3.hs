module Arkham.Asset.Cards.ArchaicGlyphsGuidingStones3 (
  archaicGlyphsGuidingStones3,
  ArchaicGlyphsGuidingStones3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Projection

newtype ArchaicGlyphsGuidingStones3 = ArchaicGlyphsGuidingStones3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities ArchaicGlyphsGuidingStones3 where
  getAbilities (ArchaicGlyphsGuidingStones3 a) =
    [investigateAbility a 1 (Costs [ActionCost 1, assetUseCost a Charge 1]) ControlsThis]

archaicGlyphsGuidingStones3 :: AssetCard ArchaicGlyphsGuidingStones3
archaicGlyphsGuidingStones3 =
  asset ArchaicGlyphsGuidingStones3 Cards.archaicGlyphsGuidingStones3

instance RunMessage ArchaicGlyphsGuidingStones3 where
  runMessage msg a@(ArchaicGlyphsGuidingStones3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> do
        skillType <- field LocationInvestigateSkill lid
        push $ Investigate iid lid (toSource attrs) (Just $ toTarget attrs) skillType False
      pure a
    Successful (Action.Investigate, LocationTarget lid) iid _ (isTarget attrs -> True) n -> do
      clueCount <- field LocationClues lid
      let
        additional = n `div` 2
        amount = min clueCount (1 + additional)
      push $ InvestigatorDiscoverClues iid lid (toAbilitySource attrs 1) amount $ Just Action.Investigate
      pure a
    _ -> ArchaicGlyphsGuidingStones3 <$> runMessage msg attrs
