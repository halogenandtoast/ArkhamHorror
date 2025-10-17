module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheWellspringOfFortune (theWellspringOfFortune) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted

newtype TheWellspringOfFortune = TheWellspringOfFortune ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWellspringOfFortune :: ScarletKeyCard TheWellspringOfFortune
theWellspringOfFortune = key TheWellspringOfFortune Cards.theWellspringOfFortune

instance HasAbilities TheWellspringOfFortune where
  getAbilities (TheWellspringOfFortune a) = case a.bearer of
    InvestigatorTarget _iid -> []
    _ -> []

instance RunMessage TheWellspringOfFortune where
  runMessage msg k@(TheWellspringOfFortune attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[88045]" Null) k
    CampaignSpecific "shift[88045]" _ -> do
      shiftKey attrs do
        when attrs.unstable $ pure ()
        when attrs.stable $ pure ()
      pure k
    _ -> TheWellspringOfFortune <$> liftRunMessage msg attrs
