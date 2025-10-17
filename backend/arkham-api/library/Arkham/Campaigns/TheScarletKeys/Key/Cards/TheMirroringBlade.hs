module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheMirroringBlade (theMirroringBlade) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted

newtype TheMirroringBlade = TheMirroringBlade ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMirroringBlade :: ScarletKeyCard TheMirroringBlade
theMirroringBlade = key TheMirroringBlade Cards.theMirroringBlade

instance HasAbilities TheMirroringBlade where
  getAbilities (TheMirroringBlade a) = case a.bearer of
    InvestigatorTarget _iid -> []
    _ -> []

instance RunMessage TheMirroringBlade where
  runMessage msg k@(TheMirroringBlade attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09768]" Null) k
    CampaignSpecific "shift[09768]" _ -> do
      shiftKey attrs do
        when attrs.unstable $ pure ()
        when attrs.stable $ pure ()
      pure k
    _ -> TheMirroringBlade <$> liftRunMessage msg attrs
