module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheRuinousChime (theRuinousChime) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted

newtype TheRuinousChime = TheRuinousChime ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRuinousChime :: ScarletKeyCard TheRuinousChime
theRuinousChime = key TheRuinousChime Cards.theRuinousChime

instance HasAbilities TheRuinousChime where
  getAbilities (TheRuinousChime a) = case a.bearer of
    InvestigatorTarget _iid -> []
    _ -> []

instance RunMessage TheRuinousChime where
  runMessage msg k@(TheRuinousChime attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09770]" Null) k
    CampaignSpecific "shift[09770]" _ -> do
      shiftKey attrs do
        when attrs.unstable $ pure ()
        when attrs.stable $ pure ()
      pure k
    _ -> TheRuinousChime <$> liftRunMessage msg attrs
