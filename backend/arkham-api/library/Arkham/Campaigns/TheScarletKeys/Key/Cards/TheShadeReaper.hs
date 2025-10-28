module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheShadeReaper (theShadeReaper) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted

newtype TheShadeReaper = TheShadeReaper ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShadeReaper :: ScarletKeyCard TheShadeReaper
theShadeReaper = key TheShadeReaper Cards.theShadeReaper

instance HasAbilities TheShadeReaper where
  getAbilities (TheShadeReaper a) = case a.bearer of
    InvestigatorTarget _iid -> []
    _ -> []

instance RunMessage TheShadeReaper where
  runMessage msg k@(TheShadeReaper attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09680]" Null) k
    CampaignSpecific "shift[09680]" _ -> do
      shiftKey attrs do
        when attrs.unstable $ pure ()
        when attrs.stable $ pure ()
      pure k
    _ -> TheShadeReaper <$> liftRunMessage msg attrs
