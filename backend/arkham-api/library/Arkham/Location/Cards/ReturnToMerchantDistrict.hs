module Arkham.Location.Cards.ReturnToMerchantDistrict (returnToMerchantDistrict) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype ReturnToMerchantDistrict = ReturnToMerchantDistrict LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToMerchantDistrict :: LocationCard ReturnToMerchantDistrict
returnToMerchantDistrict = location ReturnToMerchantDistrict Cards.returnToMerchantDistrict 4 (Static 0)

instance HasAbilities ReturnToMerchantDistrict where
  getAbilities (ReturnToMerchantDistrict a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a (LocationWithBreaches $ atLeast 1)) actionAbility

instance RunMessage ReturnToMerchantDistrict where
  runMessage msg l@(ReturnToMerchantDistrict attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <-
        select
          $ assetControlledBy iid
          <> mapOneOf (AssetWithTokens (atLeast 1)) [#charge, #supply, #secret, #ammo]

      charges <- sum <$> traverse (fieldMap AssetUses (findWithDefault 0 #charge)) assets
      secrets <- sum <$> traverse (fieldMap AssetUses (findWithDefault 0 #secret)) assets
      supply <- sum <$> traverse (fieldMap AssetUses (findWithDefault 0 #supply)) assets
      ammo <- sum <$> traverse (fieldMap AssetUses (findWithDefault 0 #ammo)) assets
      let n = countLocationBreaches attrs

      if (charges + secrets + supply + ammo) >= 1 && n > 1
        then
          scenarioI18n
            $ chooseAmounts
              iid
              (ikey' "label.merchantDistrict.choose")
              (MaxAmountTarget (n - 1))
              ( [("$charge", (0, charges)) | charges > 0]
                  ++ [("$supply", (0, supply)) | supply > 0]
                  ++ [("$secret", (0, secrets)) | secrets > 0]
                  ++ [("$ammo", (0, ammo)) | ammo > 0]
              )
              attrs
        else do
          act <- selectJust AnyAct
          removeBreaches attrs 1
          placeBreaches act 1
      pure l
    ResolveAmounts _iid amounts (isTarget attrs -> True) -> do
      let
        charge = getChoiceAmount "$charge" amounts
        supply = getChoiceAmount "$supply" amounts
        secret = getChoiceAmount "$secret" amounts
        ammo = getChoiceAmount "$ammo" amounts

      let total = charge + supply + secret + ammo + 1
      doStep total msg
      pure l
    DoStep total (ResolveAmounts iid amounts t@(isTarget attrs -> True)) -> do
      let
        charge = getChoiceAmount "$charge" amounts
        supply = getChoiceAmount "$supply" amounts
        secret = getChoiceAmount "$secret" amounts
        ammo = getChoiceAmount "$ammo" amounts

      let current = charge + supply + secret + ammo

      if current == 0
        then do
          act <- selectJust AnyAct
          removeBreaches attrs total
          placeBreaches act total
        else do
          let
            findAssets fld tkn =
              if fld > 0 then select (assetControlledBy iid <> AssetWithTokens (atLeast 1) tkn) else pure []
          chargeAssets <- findAssets charge #charge
          supplyAssets <- findAssets supply #supply
          secretAssets <- findAssets secret #secret
          ammoAssets <- findAssets ammo #ammo

          let allAssets = nub $ chargeAssets ++ supplyAssets ++ secretAssets ++ ammoAssets

          let
            updateAmounts key =
              flip mapMaybe amounts \(nu, n) ->
                if nuName nu == key
                  then guard (n - 1 > 0) $> (nu, n - 1)
                  else Just (nu, n)

          chooseTargetM iid allAssets \asset -> do
            chooseOrRunOneM iid $ scenarioI18n do
              when (asset `elem` chargeAssets) do
                labeled' "merchantDistrict.removeCharge" do
                  spendUses (attrs.ability 1) asset #charge 1
                  push $ DoStep total (ResolveAmounts iid (updateAmounts "$charge") t)
              when (asset `elem` supplyAssets) do
                labeled' "merchantDistrict.removeSupply" do
                  spendUses (attrs.ability 1) asset #supply 1
                  push $ DoStep total (ResolveAmounts iid (updateAmounts "$supply") t)
              when (asset `elem` secretAssets) do
                labeled' "merchantDistrict.removeSecret" do
                  spendUses (attrs.ability 1) asset #secret 1
                  push $ DoStep total (ResolveAmounts iid (updateAmounts "$secret") t)
              when (asset `elem` ammoAssets) do
                labeled' "merchantDistrict.removeAmmo" do
                  spendUses (attrs.ability 1) asset #ammo 1
                  push $ DoStep total (ResolveAmounts iid (updateAmounts "$ammo") t)
      pure l
    _ -> ReturnToMerchantDistrict <$> liftRunMessage msg attrs
