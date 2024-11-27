module Arkham.Asset.Assets.DanforthBrilliantStudent (
  danforthBrilliantStudent,
  DanforthBrilliantStudent (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Matcher

newtype DanforthBrilliantStudent = DanforthBrilliantStudent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danforthBrilliantStudent :: AssetCard DanforthBrilliantStudent
danforthBrilliantStudent = allyWith DanforthBrilliantStudent Cards.danforthBrilliantStudent (2, 4) noSlots

instance HasAbilities DanforthBrilliantStudent where
  getAbilities (DanforthBrilliantStudent x) =
    [ controlled x 1 (can.draw.cards You)
        $ ReactionAbility
          (ResolvesTreachery #after You $ TreacheryWithTitle "Tekeli-li")
          (assetUseCost x Secret 1 <> exhaust x)
    ]

instance RunMessage DanforthBrilliantStudent where
  runMessage msg a@(DanforthBrilliantStudent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 2
      pure a
    _ -> DanforthBrilliantStudent <$> liftRunMessage msg attrs
