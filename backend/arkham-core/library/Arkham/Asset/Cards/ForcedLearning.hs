module Arkham.Asset.Cards.ForcedLearning (forcedLearning, ForcedLearning (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Draw.Types
import Arkham.Prelude

newtype ForcedLearning = ForcedLearning AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forcedLearning :: AssetCard ForcedLearning
forcedLearning = asset ForcedLearning Cards.forcedLearning

instance HasModifiersFor ForcedLearning where
  getModifiersFor (InvestigatorTarget iid) (ForcedLearning a) | a `controlledBy` iid = do
    pure $ toModifiers a [AlternateUpkeepDraw (toTarget a)]
  getModifiersFor _ _ = pure []

instance RunMessage ForcedLearning where
  runMessage msg a@(ForcedLearning attrs) = case msg of
    SendMessage (isTarget attrs -> True) AllDrawCardAndResource -> do
      for_ attrs.controller \iid -> do
        let drawing = newCardDraw ScenarioSource iid 2
        push $ DrawCards iid $ drawing {cardDrawRules = singleton (AfterDrawDiscard 1)}
      pure a
    _ -> ForcedLearning <$> runMessage msg attrs
