module Arkham.Asset.Assets.TheNecronomiconAdvanced (TheNecronomiconAdvanced (..), theNecronomiconAdvanced) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype TheNecronomiconAdvanced = TheNecronomiconAdvanced AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomiconAdvanced :: AssetCard TheNecronomiconAdvanced
theNecronomiconAdvanced =
  assetWith TheNecronomiconAdvanced Cards.theNecronomiconAdvanced
    $ (tokensL %~ setTokens Horror 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor TheNecronomiconAdvanced where
  getModifiersFor (TheNecronomiconAdvanced a) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      modifySelect
        a
        (ChaosTokenRevealedBy $ InvestigatorWithId iid)
        [ForcedChaosTokenChange #eldersign [#cultist, #tablet, #elderthing]]

instance HasAbilities TheNecronomiconAdvanced where
  getAbilities (TheNecronomiconAdvanced a) = [controlledAbility a 1 AnyHorrorOnThis actionAbility]

instance RunMessage TheNecronomiconAdvanced where
  runMessage msg a@(TheNecronomiconAdvanced attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      push $ assignDamage iid source 1
      if assetHorror attrs <= 1
        then do
          push $ toDiscardBy iid source attrs
          pure a
        else pure $ TheNecronomiconAdvanced (attrs & tokensL %~ decrementTokens Horror)
    _ -> TheNecronomiconAdvanced <$> runMessage msg attrs
