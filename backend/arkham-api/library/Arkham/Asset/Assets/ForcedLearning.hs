module Arkham.Asset.Assets.ForcedLearning (forcedLearning) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Draw.Types
import Arkham.Helpers.Modifiers
import Arkham.Prelude

newtype ForcedLearning = ForcedLearning AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forcedLearning :: AssetCard ForcedLearning
forcedLearning = asset ForcedLearning Cards.forcedLearning

instance HasModifiersFor ForcedLearning where
  getModifiersFor (ForcedLearning a) = controllerGets a [AlternateUpkeepDraw (toTarget a)]

instance RunMessage ForcedLearning where
  runMessage msg a@(ForcedLearning attrs) = case msg of
    SendMessage (isTarget attrs -> True) (ForInvestigator iid AllDrawCardAndResource) | Just iid == attrs.controller -> do
      let drawing = newCardDraw ScenarioSource iid 2
      push $ DrawCards iid $ drawing {cardDrawRules = singleton (AfterDrawDiscard 1)}
      pure a
    _ -> ForcedLearning <$> runMessage msg attrs
