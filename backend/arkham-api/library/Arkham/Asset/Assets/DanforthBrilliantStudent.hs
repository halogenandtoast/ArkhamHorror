module Arkham.Asset.Assets.DanforthBrilliantStudent (danforthBrilliantStudent) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Helpers.Window (getTreacheryResolver)
import Arkham.Matcher

newtype DanforthBrilliantStudent = DanforthBrilliantStudent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danforthBrilliantStudent :: AssetCard DanforthBrilliantStudent
danforthBrilliantStudent = allyWith DanforthBrilliantStudent Cards.danforthBrilliantStudent (2, 4) noSlots

instance HasAbilities DanforthBrilliantStudent where
  getAbilities (DanforthBrilliantStudent x) =
    [ controlled x 1 (can.draw.cards (affectsColocatedMatch You))
        $ triggered
          (ResolvesTreachery #after (affectsColocatedMatch You) "Tekeli-li")
          (assetUseCost x Secret 1 <> exhaust x)
    ]

instance RunMessage DanforthBrilliantStudent where
  runMessage msg a@(DanforthBrilliantStudent attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getTreacheryResolver -> iid) _ -> do
      drawCards iid (attrs.ability 1) 2
      pure a
    _ -> DanforthBrilliantStudent <$> liftRunMessage msg attrs
