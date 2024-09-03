module Arkham.Asset.Cards.HeirloomOfHyperboreaAdvanced (
  heirloomOfHyperboreaAdvanced,
  HeirloomOfHyperboreaAdvanced (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Matcher
import Arkham.Modifier

newtype HeirloomOfHyperboreaAdvanced = HeirloomOfHyperboreaAdvanced AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirloomOfHyperboreaAdvanced :: AssetCard HeirloomOfHyperboreaAdvanced
heirloomOfHyperboreaAdvanced =
  assetWith
    HeirloomOfHyperboreaAdvanced
    Cards.heirloomOfHyperboreaAdvanced
    ((healthL ?~ 3) . (sanityL ?~ 3))

instance HasModifiersFor HeirloomOfHyperboreaAdvanced where
  getModifiersFor target (HeirloomOfHyperboreaAdvanced a) | isTarget a target = do
    modified a [CannotBeDamagedBySourcesExcept $ SourceIsPlayerCard <> SourceIsCardEffect]
  getModifiersFor _ _ = pure []

instance HasAbilities HeirloomOfHyperboreaAdvanced where
  getAbilities (HeirloomOfHyperboreaAdvanced x) =
    [controlledAbility x 1 CanDrawCards $ freeReaction $ PlayCard #after You #spell]

instance RunMessage HeirloomOfHyperboreaAdvanced where
  runMessage msg a@(HeirloomOfHyperboreaAdvanced attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> HeirloomOfHyperboreaAdvanced <$> liftRunMessage msg attrs
