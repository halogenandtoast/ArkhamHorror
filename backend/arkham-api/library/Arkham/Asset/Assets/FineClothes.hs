module Arkham.Asset.Assets.FineClothes (fineClothes, FineClothes (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Prelude

newtype FineClothes = FineClothes AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fineClothes :: AssetCard FineClothes
fineClothes = assetWith FineClothes Cards.fineClothes $ (healthL ?~ 1) . (sanityL ?~ 1)

instance HasModifiersFor FineClothes where
  getModifiersFor (SkillTestTarget _) (FineClothes a) = do
    -- because this is during a parley we can check if any active abilities are parleys
    inParley <- any (`abilityIs` #parley) <$> getActiveAbilities
    maybeModified a do
      Action.Parley <- MaybeT $ if inParley then pure (Just Action.Parley) else getSkillTestAction
      iid <- MaybeT getSkillTestInvestigator
      guard $ a `controlledBy` iid
      pure [Difficulty (-2)]
  getModifiersFor _ _ = pure []

instance RunMessage FineClothes where
  runMessage msg (FineClothes attrs) = FineClothes <$> runMessage msg attrs
