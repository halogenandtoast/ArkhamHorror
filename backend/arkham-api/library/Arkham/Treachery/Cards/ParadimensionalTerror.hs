module Arkham.Treachery.Cards.ParadimensionalTerror (paradimensionalTerror) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ParadimensionalTerror = ParadimensionalTerror TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paradimensionalTerror :: TreacheryCard ParadimensionalTerror
paradimensionalTerror = treachery ParadimensionalTerror Cards.paradimensionalTerror

instance RunMessage ParadimensionalTerror where
  runMessage msg t@(ParadimensionalTerror attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ campaignI18n do
        labeled' "paradimensionalTerror.shuffleAllConcealed" $ scenarioSpecific_ "shuffleAllConcealed"
        labeled' "paradimensionalTerror.takeDamageAndHorror" do
          eachInvestigator \iid' -> assignDamageAndHorror iid' attrs 1 1
      pure t
    _ -> ParadimensionalTerror <$> liftRunMessage msg attrs
