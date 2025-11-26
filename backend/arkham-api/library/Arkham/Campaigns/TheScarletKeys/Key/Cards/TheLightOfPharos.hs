module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheLightOfPharos (theLightOfPharos) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted

newtype TheLightOfPharos = TheLightOfPharos ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLightOfPharos :: ScarletKeyCard TheLightOfPharos
theLightOfPharos = key TheLightOfPharos Cards.theLightOfPharos

instance HasAbilities TheLightOfPharos where
  getAbilities (TheLightOfPharos a) = case a.bearer of
    InvestigatorTarget _iid | not a.shifted -> []
    _ -> []

instance RunMessage TheLightOfPharos where
  runMessage msg k@(TheLightOfPharos attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09659]" Null) k
    CampaignSpecific "shift[09659]" _ -> do
      shiftKey attrs do
        when attrs.unstable $ pure ()
        when attrs.stable $ pure ()
      pure k
    _ -> TheLightOfPharos <$> liftRunMessage msg attrs
