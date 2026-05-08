module Arkham.Asset.Assets.CenterStage (centerStage) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.History
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype CenterStage = CenterStage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

centerStage :: AssetCard CenterStage
centerStage = assetWith CenterStage Cards.centerStage discardWhenNoUses

instance HasAbilities CenterStage where
  getAbilities (CenterStage a) =
    [ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ assetUseCost a Renown 1 <> exhaust a)
    ]

instance RunMessage CenterStage where
  runMessage msg a@(CenterStage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        spent <- getHistoryField RoundHistory iid HistoryActionsSpent
        skillTestModifier sid attrs iid (AnySkillValue (min 3 spent))
      pure a
    _ -> CenterStage <$> liftRunMessage msg attrs
