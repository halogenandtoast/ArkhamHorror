module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheSableGlass (theSableGlass) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted

newtype TheSableGlass = TheSableGlass ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSableGlass :: ScarletKeyCard TheSableGlass
theSableGlass = key TheSableGlass Cards.theSableGlass

instance HasAbilities TheSableGlass where
  getAbilities (TheSableGlass a) = case a.bearer of
    InvestigatorTarget _iid -> []
    _ -> []

instance RunMessage TheSableGlass where
  runMessage msg k@(TheSableGlass attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09634]" Null) k
    CampaignSpecific "shift[09634]" _ -> do
      shiftKey attrs do
        when attrs.unstable $ pure ()
        when attrs.stable $ pure ()
      pure k
    _ -> TheSableGlass <$> liftRunMessage msg attrs
