module Arkham.Asset.Cards.TheNecronomiconAdvanced (TheNecronomiconAdvanced (..), theNecronomiconAdvanced) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken qualified as Token
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
  getModifiersFor (InvestigatorTarget iid) (TheNecronomiconAdvanced a) =
    pure
      $ toModifiers
        a
        [ ForcedChaosTokenChange
          Token.ElderSign
          [Token.Cultist, Token.Tablet, Token.ElderThing]
        | controlledBy a iid
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities TheNecronomiconAdvanced where
  getAbilities (TheNecronomiconAdvanced a) = [controlledAbility a 1 AnyHorrorOnThis actionAbility]

instance RunMessage TheNecronomiconAdvanced where
  runMessage msg a@(TheNecronomiconAdvanced attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      push $ InvestigatorDamage iid source 0 1
      if assetHorror attrs <= 1
        then do
          push $ toDiscardBy iid source attrs
          pure a
        else pure $ TheNecronomiconAdvanced (attrs & tokensL %~ decrementTokens Horror)
    _ -> TheNecronomiconAdvanced <$> runMessage msg attrs
