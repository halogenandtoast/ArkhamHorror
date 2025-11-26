module Arkham.Asset.Assets.InspectorFlintWithPrideAndCare (inspectorFlintWithPrideAndCare) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Capability
import Arkham.Helpers.Location (getLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window qualified as Window

newtype InspectorFlintWithPrideAndCare = InspectorFlintWithPrideAndCare AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inspectorFlintWithPrideAndCare :: AssetCard InspectorFlintWithPrideAndCare
inspectorFlintWithPrideAndCare = ally InspectorFlintWithPrideAndCare Cards.inspectorFlintWithPrideAndCare (3, 3)

instance HasAbilities InspectorFlintWithPrideAndCare where
  getAbilities (InspectorFlintWithPrideAndCare a) =
    [controlled_ a 1 $ triggered (CampaignEvent #when (Just You) "exposed[decoy]") (exhaust a)]

instance RunMessage InspectorFlintWithPrideAndCare where
  runMessage msg a@(InspectorFlintWithPrideAndCare attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      cancelWindowBatch ws
      let
        go = \case
          [] -> pure ()
          ((Window.windowType -> Window.CampaignEvent "exposed[decoy]" _ value) : _) -> do
            case maybeResult value of
              Just (ConcealedCardTarget cid) -> do
                others <- maybe (pure []) (select . ConcealedCardAt . LocationWithId) =<< getLocationOf cid
                flipOverBy iid (attrs.ability 1) cid
                chooseOneM iid do
                  whenM (can.draw.cards iid) do
                    withI18n $ countVar 1 $ labeled' "drawCards" $ drawCards iid (attrs.ability 1) 1
                  unless (null others) do
                    campaignI18n $ labeled' "inspectorFlint.expose" do
                      chooseTargetM iid others $ exposeConcealed iid (attrs.ability 1) . toId
              _ -> pure ()
          (_ : xs) -> go xs
      go ws
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    _ -> InspectorFlintWithPrideAndCare <$> liftRunMessage msg attrs
