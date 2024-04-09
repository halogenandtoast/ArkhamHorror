module Arkham.Asset.Cards.HenryWan (henryWan, HenryWan (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy

newtype Metadata = Metadata {revealedChaosTokens :: [ChaosToken]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HenryWan = HenryWan (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

henryWan :: AssetCard HenryWan
henryWan = ally (HenryWan . (`with` Metadata [])) Cards.henryWan (1, 2)

instance HasAbilities HenryWan where
  getAbilities (HenryWan (a `With` _)) = [restrictedAbility a 1 ControlsThis $ actionAbilityWithCost (exhaust a)]

validToken :: ChaosToken -> Bool
validToken = (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . chaosTokenFace

instance RunMessage HenryWan where
  runMessage msg a@(HenryWan (attrs `With` meta)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal 1) SetAside
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      player <- getPlayer iid
      let source = attrs.ability 1
      push
        $ chooseOne player
        $ if any validToken tokens
          then
            [Label "Do nothing" [HandleTargetChoice iid source (toTarget attrs)]]
          else
            [ Label "Stop" [HandleTargetChoice iid source (toTarget attrs)]
            , Label "Draw Another" [RequestChaosTokens source (Just iid) (Reveal 1) SetAside]
            ]
      pure $ HenryWan (attrs `with` Metadata (tokens <> revealedChaosTokens meta))
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) _ -> do
      push $ ResetChaosTokens (toAbilitySource attrs 1)
      unless (any validToken (revealedChaosTokens meta)) do
        let source = attrs.ability 1
        mDrawing <- drawCardsIfCan iid source 1
        mGainResources <- gainResourcesIfCan iid source 1
        when (isJust mDrawing || isJust mGainResources) do
          player <- getPlayer iid
          msgs <- for (revealedChaosTokens meta) \_ -> do
            pure
              $ chooseOrRunOne player
              $ [Label "Draw 1 card" [drawing] | drawing <- toList mDrawing]
              <> [Label "Gain 1 resources" [gain] | gain <- toList mGainResources]
          pushAll msgs
      pure $ HenryWan (attrs `with` Metadata [])
    _ -> HenryWan . (`with` meta) <$> runMessage msg attrs
