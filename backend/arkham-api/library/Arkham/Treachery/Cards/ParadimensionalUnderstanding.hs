module Arkham.Treachery.Cards.ParadimensionalUnderstanding (paradimensionalUnderstanding) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ParadimensionalUnderstanding = ParadimensionalUnderstanding TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paradimensionalUnderstanding :: TreacheryCard ParadimensionalUnderstanding
paradimensionalUnderstanding = treachery ParadimensionalUnderstanding Cards.paradimensionalUnderstanding

instance RunMessage ParadimensionalUnderstanding where
  runMessage msg t@(ParadimensionalUnderstanding attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      unstableKeys <- select $ ScarletKeyWithInvestigator (InvestigatorWithId iid) <> UnstableScarletKey
      stableKeys <- select $ ScarletKeyWithInvestigator (InvestigatorWithId iid) <> StableScarletKey
      chooseOneM iid $ campaignI18n do
        labeled' "paradimensionalUnderstanding.unstable" do
          chooseTargetM iid unstableKeys shift
        labeled' "paradimensionalUnderstanding.stable" do
          for_ stableKeys $ flipOverBy iid attrs
      pure t
    _ -> ParadimensionalUnderstanding <$> liftRunMessage msg attrs
