module Arkham.Asset.Assets.HenryWan (henryWan) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.ChaosToken
import Arkham.Message.Lifted.Choose

newtype Metadata = Metadata {revealedChaosTokens :: [ChaosToken]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HenryWan = HenryWan (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

henryWan :: AssetCard HenryWan
henryWan = ally (HenryWan . (`with` Metadata [])) Cards.henryWan (1, 2)

instance HasAbilities HenryWan where
  getAbilities (HenryWan (a `With` _)) = [controlled_ a 1 $ actionAbilityWithCost (exhaust a)]

invalidToken :: ChaosToken -> Bool
invalidToken = (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . chaosTokenFace

instance RunMessage HenryWan where
  runMessage msg a@(HenryWan (attrs `With` meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) 1
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      let source = attrs.ability 1
      chooseOneM iid do
        if any invalidToken tokens
          then labeled "Do nothing" $ handleTarget iid source attrs
          else do
            labeled "Stop" $ handleTarget iid source attrs
            labeled "Draw Another" $ requestChaosTokens iid source 1
      pure $ HenryWan (attrs `with` Metadata (tokens <> revealedChaosTokens meta))
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) _ -> do
      resetChaosTokens (attrs.ability 1)
      unfocusChaosTokens
      for_ (filter (not . invalidToken) (revealedChaosTokens meta)) (`forTarget_` msg)
      pure $ HenryWan (attrs `with` Metadata [])
    ForTarget (ChaosTokenTarget _) (HandleTargetChoice iid (isAbilitySource attrs 1 -> True) _) -> do
      drawOk <- can.draw.cards iid
      resourceOk <- can.gain.resources iid
      when (drawOk || resourceOk) do
        chooseOrRunOneM iid do
          when drawOk $ labeled "Draw 1 card" $ drawCards iid (attrs.ability 1) 1
          when resourceOk $ labeled "Gain 1 resources" $ gainResources iid (attrs.ability 1) 1
      pure a
    _ -> HenryWan . (`with` meta) <$> liftRunMessage msg attrs
