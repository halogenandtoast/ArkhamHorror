module Arkham.Asset.Cards.FavorOfTheMoon1 (favorOfTheMoon1, FavorOfTheMoon1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window

newtype FavorOfTheMoon1 = FavorOfTheMoon1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

favorOfTheMoon1 :: AssetCard FavorOfTheMoon1
favorOfTheMoon1 = assetWith FavorOfTheMoon1 Cards.favorOfTheMoon1 $ setMeta False

instance HasAbilities FavorOfTheMoon1 where
  getAbilities (FavorOfTheMoon1 attrs) =
    let active = toResult @Bool attrs.meta
     in restrictedAbility
          attrs
          1
          ControlsThis
          (ReactionAbility (WouldRevealChaosToken #when You) (exhaust attrs))
          : [restrictedAbility attrs 2 (thisExists attrs AssetWithoutSealedTokens) Anytime | active]

instance RunMessage FavorOfTheMoon1 where
  runMessage msg a@(FavorOfTheMoon1 attrs) = runQueueT $ case msg of
    ResolvedCard _ card | toCardId card == toCardId attrs -> do
      FavorOfTheMoon1 <$> lift (runMessage msg $ setMeta True attrs)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      focusChaosTokens attrs.sealedChaosTokens \unfocus -> do
        chooseOne
          iid
          [ targetLabel token [unfocus, handleTargetChoice iid (attrs.ability 1) token]
          | token <- attrs.sealedChaosTokens
          ]

      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (ChaosTokenTarget token) -> do
      cancelTokenDraw
      push $ SetChaosTokenAside token
      checkWindows [mkWhen (Window.RevealChaosToken iid token)]
      withSkillTest \sid ->
        push $ RequestedChaosTokens (SkillTestSource sid) (Just iid) [token]
      gainResourcesIfCan iid (attrs.ability 1) 1

      pure $ FavorOfTheMoon1 $ attrs & sealedChaosTokensL %~ filter (/= token)
    _ -> FavorOfTheMoon1 <$> liftRunMessage msg attrs
