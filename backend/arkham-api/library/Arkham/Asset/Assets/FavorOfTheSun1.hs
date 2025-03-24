module Arkham.Asset.Assets.FavorOfTheSun1 (favorOfTheSun1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window qualified as Window

newtype FavorOfTheSun1 = FavorOfTheSun1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

favorOfTheSun1 :: AssetCard FavorOfTheSun1
favorOfTheSun1 = assetWith FavorOfTheSun1 Cards.favorOfTheSun1 $ setMeta False

instance HasAbilities FavorOfTheSun1 where
  getAbilities (FavorOfTheSun1 a) =
    restricted a 1 ControlsThis (triggered (WouldRevealChaosToken #when You) (exhaust a))
      : [controlled a 2 (thisExists a AssetWithoutSealedTokens) Anytime | active]
   where
    active = toResult @Bool a.meta

instance RunMessage FavorOfTheSun1 where
  runMessage msg a@(FavorOfTheSun1 attrs) = runQueueT $ case msg of
    ResolvedCard _ (sameCard attrs -> True) -> do
      FavorOfTheSun1 <$> liftRunMessage msg (setMeta True attrs)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      focusChaosTokens attrs.sealedChaosTokens \unfocus -> do
        chooseTargetM iid attrs.sealedChaosTokens \token -> do
          push unfocus
          handleTarget iid (attrs.ability 1) token
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure $ FavorOfTheSun1 $ setMeta False attrs
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (ChaosTokenTarget token) -> do
      cancelTokenDraw
      push $ SetChaosTokenAside token
      checkWhen $ Window.RevealChaosToken iid token
      withSkillTest \sid ->
        push $ RequestedChaosTokens (SkillTestSource sid) (Just iid) [token]
      gainResources iid (attrs.ability 1) 1

      pure $ FavorOfTheSun1 $ attrs & sealedChaosTokensL %~ filter (/= token)
    _ -> FavorOfTheSun1 <$> liftRunMessage msg attrs
