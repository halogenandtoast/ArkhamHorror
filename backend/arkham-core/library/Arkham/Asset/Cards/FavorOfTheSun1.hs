module Arkham.Asset.Cards.FavorOfTheSun1 (favorOfTheSun1, FavorOfTheSun1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Matcher
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window

newtype FavorOfTheSun1 = FavorOfTheSun1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

favorOfTheSun1 :: AssetCard FavorOfTheSun1
favorOfTheSun1 = assetWith FavorOfTheSun1 Cards.favorOfTheSun1 $ setMeta False

instance HasAbilities FavorOfTheSun1 where
  getAbilities (FavorOfTheSun1 attrs) =
    let active = toResult @Bool attrs.meta
     in restrictedAbility
          attrs
          1
          ControlsThis
          (ReactionAbility (WouldRevealChaosToken #when You) (exhaust attrs))
          : [restrictedAbility attrs 2 (thisExists attrs AssetWithoutSealedTokens) Anytime | active]

instance RunMessage FavorOfTheSun1 where
  runMessage msg a@(FavorOfTheSun1 attrs) = runQueueT $ case msg of
    ResolvedCard _ card | toCardId card == toCardId attrs -> do
      FavorOfTheSun1 <$> lift (runMessage msg $ setMeta True attrs)
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
      push $ RequestedChaosTokens SkillTestSource (Just iid) [token]
      gainResourcesIfCan iid (attrs.ability 1) 1

      pure $ FavorOfTheSun1 $ attrs & sealedChaosTokensL %~ filter (/= token)
    _ -> FavorOfTheSun1 <$> liftRunMessage msg attrs
