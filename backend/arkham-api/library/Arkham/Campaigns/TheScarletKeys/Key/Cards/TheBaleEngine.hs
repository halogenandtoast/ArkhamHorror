module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheBaleEngine (theBaleEngine) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted

newtype TheBaleEngine = TheBaleEngine ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBaleEngine :: ScarletKeyCard TheBaleEngine
theBaleEngine = key TheBaleEngine Cards.theBaleEngine

instance HasAbilities TheBaleEngine where
  getAbilities (TheBaleEngine a) = case a.bearer of
    InvestigatorTarget _iid -> []
    _ -> []

instance RunMessage TheBaleEngine where
  runMessage msg k@(TheBaleEngine attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09769]" Null) k
    CampaignSpecific "shift[09769]" _ -> do
      shiftKey attrs do
        when attrs.unstable $ pure ()
        when attrs.stable $ pure ()
      pure k
    _ -> TheBaleEngine <$> liftRunMessage msg attrs
