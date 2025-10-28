module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheWeepingLady (theWeepingLady) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.ForMovement
import Arkham.Matcher hiding (key)
import Arkham.Message.Lifted.Choose
import Control.Monad.State.Strict (execStateT, put)

newtype TheWeepingLady = TheWeepingLady ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWeepingLady :: ScarletKeyCard TheWeepingLady
theWeepingLady = key TheWeepingLady Cards.theWeepingLady

instance HasAbilities TheWeepingLady where
  getAbilities (TheWeepingLady a) = case a.bearer of
    InvestigatorTarget iid ->
      if a.stable
        then
          [ restricted
              a
              1
              (exists (orConnected NotForMovement YourLocation <> locationWithDiscoverableCluesBy iid))
              $ FastAbility Free
          ]
        else
          [ restricted
              a
              1
              (exists (AssetControlledBy Anyone <> DiscardableAsset))
              $ FastAbility Free
          ]
    _ -> []

instance RunMessage TheWeepingLady where
  runMessage msg k@(TheWeepingLady attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09565]" Null) k
    CampaignSpecific "shift[09565]" _ -> do
      shiftKey attrs do
        when attrs.unstable do
          willDiscard <- flip execStateT False do
            eachInvestigator \iid -> do
              assets <- select $ assetControlledBy iid <> DiscardableAsset
              unless (null assets) $ put True
              chooseTargetM iid assets $ \asset -> toDiscardBy iid attrs asset
          when willDiscard $ withInvestigatorBearer attrs (`flipOver` attrs)
        when attrs.stable do
          withInvestigatorBearer attrs \iid -> do
            locations <-
              select
                $ orConnected NotForMovement (locationWithInvestigator iid)
                <> locationWithDiscoverableCluesBy iid
            chooseTargetM iid locations $ discoverAt NotInvestigate iid attrs 1
            flipOver iid attrs
      pure k
    _ -> TheWeepingLady <$> liftRunMessage msg attrs
