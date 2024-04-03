module Arkham.Asset.Cards.TheNecronomiconAdvanced (TheNecronomiconAdvanced (..), theNecronomiconAdvanced) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude
import Arkham.Token

newtype TheNecronomiconAdvanced = TheNecronomiconAdvanced AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomiconAdvanced :: AssetCard TheNecronomiconAdvanced
theNecronomiconAdvanced =
  assetWith TheNecronomiconAdvanced Cards.theNecronomiconAdvanced
    $ (tokensL %~ setTokens Horror 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor TheNecronomiconAdvanced where
  getModifiersFor (ChaosTokenTarget token) (TheNecronomiconAdvanced a) = do
    case a.controller of
      Just iid | token.revealedBy == Just iid -> do
        pure $ toModifiers a [ForcedChaosTokenChange #eldersign [#cultist, #tablet, #elderthing]]
      _ -> pure []
  getModifiersFor _ _ = pure []

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
