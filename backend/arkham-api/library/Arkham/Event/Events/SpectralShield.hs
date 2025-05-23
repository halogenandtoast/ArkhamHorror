module Arkham.Event.Events.SpectralShield (spectralShield) where

import Arkham.Ability
import Arkham.Asset.Types (Field(..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Healing
import Arkham.Helpers.Window (getDamageOrHorrorSource, getTotalDamageAmounts)
import Arkham.Matcher
import Arkham.Projection
import Arkham.Token

newtype SpectralShield = SpectralShield EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralShield :: EventCard SpectralShield
spectralShield = event SpectralShield Cards.spectralShield

instance HasAbilities SpectralShield where
  getAbilities (SpectralShield a) = case a.placement of
    AttachedToInvestigator iid ->
      [ restricted a 1 ControlsThis
          $ forced
          $ DealtDamageOrHorror #when (SourceIsCancelable AnySource) (be iid)
      ]
    AttachedToAsset aid _ ->
      [ restricted a 1 ControlsThis
          $ forced
          $ AssetDealtDamageOrHorror #when (SourceIsCancelable AnySource) (AssetWithId aid)
      ]
    _ -> []

instance RunMessage SpectralShield where
  runMessage msg e@(SpectralShield attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      investigators <- select $ affectsOthers $ colocatedWith iid
      assets <- select $ at_ (locationWithInvestigator iid) <> oneOf [AssetWithHealth, AssetWithSanity]
      chooseOneM iid do
        targets investigators $ place attrs . AttachedToInvestigator
        targets assets $ place attrs . (`AttachedToAsset` Nothing)
      pure e
    UseCardAbility iid' (isSource attrs -> True) 1 ws@(getDamageOrHorrorSource -> dSource) _ -> do
      case attrs.placement of
        AttachedToInvestigator iid -> do
          (damage, horror) <- getDamageAmounts iid
          if
            | damage > 0 && horror > 0 -> chooseOneM iid do
                labeled "Cancel 1 Damage" $ push $ CancelDamage iid 1
                labeled "Cancel 1 Horror" $ push $ CancelHorror iid 1
            | damage > 0 -> push $ CancelDamage iid 1
            | horror > 0 -> push $ CancelHorror iid 1
            | otherwise -> pure ()
        AttachedToAsset aid _ -> do
          let (damage, horror) = findWithDefault (0, 0) dSource (getTotalDamageAmounts aid ws)
          iid <- fromMaybe iid' <$> field AssetOwner aid
          if
            | damage > 0 && horror > 0 -> chooseOneM iid do
                labeled "Cancel 1 Damage" $ push $ CancelAssetDamage aid dSource 1
                labeled "Cancel 1 Horror" $ push $ CancelAssetHorror aid dSource 1
            | damage > 0 -> push $ CancelAssetDamage aid dSource 1
            | horror > 0 -> push $ CancelAssetHorror aid dSource 1
            | otherwise -> pure ()
        _ -> error "unpexpected placement"

      chargeAssets <- select $ assetControlledBy attrs.owner <> AssetWithUses Charge
      if null chargeAssets
        then toDiscard (attrs.ability 1) attrs
        else chooseOneM attrs.owner do
          targets chargeAssets \chargeAsset -> removeTokens (attrs.ability 1) chargeAsset Charge 1
      pure e
    _ -> SpectralShield <$> liftRunMessage msg attrs
