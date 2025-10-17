module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheTwistedAntiprism (theTwistedAntiprism) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted

newtype TheTwistedAntiprism = TheTwistedAntiprism ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTwistedAntiprism :: ScarletKeyCard TheTwistedAntiprism
theTwistedAntiprism = key TheTwistedAntiprism Cards.theTwistedAntiprism

instance HasAbilities TheTwistedAntiprism where
  getAbilities (TheTwistedAntiprism a) = case a.bearer of
    InvestigatorTarget _iid -> []
    _ -> []

instance RunMessage TheTwistedAntiprism where
  runMessage msg k@(TheTwistedAntiprism attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09590]" Null) k
    CampaignSpecific "shift[09590]" _ -> do
      shiftKey attrs do
        when attrs.unstable $ pure ()
        when attrs.stable $ pure ()
      pure k
    _ -> TheTwistedAntiprism <$> liftRunMessage msg attrs
