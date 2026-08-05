module Arkham.Asset.Assets.ToeTheLineRestoredAndResolved (toeTheLineCompleted) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)

newtype ToeTheLineRestoredAndResolved = ToeTheLineRestoredAndResolved AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toeTheLineCompleted :: AssetCard ToeTheLineRestoredAndResolved
toeTheLineCompleted = asset ToeTheLineRestoredAndResolved Cards.toeTheLineCompleted

instance HasModifiersFor ToeTheLineRestoredAndResolved where
  getModifiersFor (ToeTheLineRestoredAndResolved a) =
    for_ a.controller \iid -> modified_ a iid [SkillModifier #combat 1, HealthModifier 2]

instance RunMessage ToeTheLineRestoredAndResolved where
  runMessage msg (ToeTheLineRestoredAndResolved attrs) =
    runQueueT $ ToeTheLineRestoredAndResolved <$> liftRunMessage msg attrs
