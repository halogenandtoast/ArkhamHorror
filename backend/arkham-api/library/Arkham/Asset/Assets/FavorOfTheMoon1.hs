module Arkham.Asset.Assets.FavorOfTheMoon1 (favorOfTheMoon1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype FavorOfTheMoon1 = FavorOfTheMoon1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

favorOfTheMoon1 :: AssetCard FavorOfTheMoon1
favorOfTheMoon1 = asset FavorOfTheMoon1 Cards.favorOfTheMoon1

instance HasAbilities FavorOfTheMoon1 where
  getAbilities (FavorOfTheMoon1 a) =
    [ controlled_ a 1 $ triggered (WouldRevealChaosToken #when You) (exhaust a)
    , controlled a 2 (thisExists a AssetWithoutSealedTokens) Anytime
    ]

instance RunMessage FavorOfTheMoon1 where
  runMessage msg a@(FavorOfTheMoon1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      focusChaosTokens attrs.sealedChaosTokens \unfocus -> do
        chooseOrRunOneM iid do
          targets attrs.sealedChaosTokens.uniqueByFace \token -> do
            push unfocus
            handleTarget iid (attrs.ability 1) token
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (ChaosTokenTarget token) -> do
      unsealChaosToken token
      forceChaosTokenDraw token
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> FavorOfTheMoon1 <$> liftRunMessage msg attrs
