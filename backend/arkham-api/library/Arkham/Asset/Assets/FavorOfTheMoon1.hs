module Arkham.Asset.Assets.FavorOfTheMoon1 (favorOfTheMoon1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window qualified as Window

newtype FavorOfTheMoon1 = FavorOfTheMoon1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

favorOfTheMoon1 :: AssetCard FavorOfTheMoon1
favorOfTheMoon1 = assetWith FavorOfTheMoon1 Cards.favorOfTheMoon1 $ setMeta False

instance HasAbilities FavorOfTheMoon1 where
  getAbilities (FavorOfTheMoon1 a) =
    restricted a 1 ControlsThis (triggered (WouldRevealChaosToken #when You) (exhaust a))
      : [controlled a 2 (thisExists a AssetWithoutSealedTokens) Anytime | active]
   where
    active = toResult @Bool a.meta

instance RunMessage FavorOfTheMoon1 where
  runMessage msg a@(FavorOfTheMoon1 attrs) = runQueueT $ case msg of
    ResolvedCard _ (sameCard attrs -> True) -> do
      FavorOfTheMoon1 <$> lift (runMessage msg $ setMeta True attrs)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      focusChaosTokens attrs.sealedChaosTokens \unfocus -> do
        chooseTargetM iid attrs.sealedChaosTokens \token -> do
          push unfocus
          handleTarget iid (attrs.ability 1) token
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (ChaosTokenTarget token) -> do
      cancelTokenDraw
      push $ SetChaosTokenAside token
      checkWhen $ Window.RevealChaosToken iid token
      withSkillTest \sid ->
        push $ RequestedChaosTokens (SkillTestSource sid) (Just iid) [token]
      gainResourcesIfCan iid (attrs.ability 1) 1

      pure $ FavorOfTheMoon1 $ attrs & sealedChaosTokensL %~ filter (/= token)
    _ -> FavorOfTheMoon1 <$> liftRunMessage msg attrs
