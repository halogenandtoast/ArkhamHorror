module Arkham.Asset.Assets.SinhasMedicalKit (sinhasMedicalKit) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SinhasMedicalKit = SinhasMedicalKit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinhasMedicalKit :: AssetCard SinhasMedicalKit
sinhasMedicalKit = asset SinhasMedicalKit Cards.sinhasMedicalKit

instance HasAbilities SinhasMedicalKit where
  getAbilities (SinhasMedicalKit a) = [controlled a 1 criteria $ FastAbility (exhaust a <> assetUseCost a Supply 1)]
   where
    healableAsset (toSource -> source) hType loc = HealableAsset source hType $ at_ loc <> AssetControlledBy (affectsOthers Anyone)
    healable hType = HealableInvestigator (toSource a) hType $ at_ YourLocation
    criteria =
      oneOf
        [ any_ $ healable <$> [#horror, #damage]
        , any_ $ map (\hType -> healableAsset a hType YourLocation) [#damage, #horror]
        ]

instance RunMessage SinhasMedicalKit where
  runMessage msg a@(SinhasMedicalKit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let
        withCanHeal kind = selectEach (HealableInvestigator (attrs.ability 1) kind (colocatedWith iid))
        withCanHealAsset kind =
          selectEach
            ( HealableAsset (attrs.ability 1) kind
                $ oneOf [UncontrolledAsset, AssetControlledBy (affectsOthers Anyone)]
                <> at_ (locationWithInvestigator iid)
            )
      chooseOneM iid do
        withCanHeal #damage \x -> damageLabeled x $ healDamage x (attrs.ability 1) 1
        withCanHeal #horror \x -> horrorLabeled x $ healHorror x (attrs.ability 1) 1

        withCanHealAsset #damage \x -> assetDamageLabeled x $ healDamage x (attrs.ability 1) 1
        withCanHealAsset #horror \x -> assetHorrorLabeled x $ healHorror x (attrs.ability 1) 1
      pure a
    _ -> SinhasMedicalKit <$> liftRunMessage msg attrs
