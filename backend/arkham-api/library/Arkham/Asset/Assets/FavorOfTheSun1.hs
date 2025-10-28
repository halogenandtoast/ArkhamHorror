module Arkham.Asset.Assets.FavorOfTheSun1 (favorOfTheSun1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype FavorOfTheSun1 = FavorOfTheSun1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

favorOfTheSun1 :: AssetCard FavorOfTheSun1
favorOfTheSun1 = asset FavorOfTheSun1 Cards.favorOfTheSun1

instance HasAbilities FavorOfTheSun1 where
  getAbilities (FavorOfTheSun1 a) =
    [ controlled_ a 1 $ triggered (WouldRevealChaosToken #when You) (exhaust a)
    , controlled a 2 (thisExists a AssetWithoutSealedTokens) Anytime
    ]

instance RunMessage FavorOfTheSun1 where
  runMessage msg a@(FavorOfTheSun1 attrs) = runQueueT $ case msg of
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
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (ChaosTokenTarget token) -> do
      unsealChaosToken token
      forceChaosTokenDraw token
      pure a
    _ -> FavorOfTheSun1 <$> liftRunMessage msg attrs
