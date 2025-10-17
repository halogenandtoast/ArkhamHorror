module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheWeepingLady (theWeepingLady) where

-- import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted

-- import Arkham.Matcher hiding (key)

newtype TheWeepingLady = TheWeepingLady ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWeepingLady :: ScarletKeyCard TheWeepingLady
theWeepingLady = key TheWeepingLady Cards.theWeepingLady

instance HasAbilities TheWeepingLady where
  getAbilities (TheWeepingLady a) = case a.bearer of
    InvestigatorTarget _iid -> []
    _ -> []

instance RunMessage TheWeepingLady where
  runMessage msg k@(TheWeepingLady attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09565]" Null) k
    CampaignSpecific "shift[09565]" _ -> do
      shiftKey attrs do
        when attrs.unstable $ pure ()
        when attrs.stable $ pure ()
      pure k
    _ -> TheWeepingLady <$> liftRunMessage msg attrs
