module Arkham.Treachery.Cards.GrossPlasticity (grossPlasticity) where

import Arkham.Campaigns.TheDrownedCity.Helpers (increaseFloodLevel)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Matcher
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GrossPlasticity = GrossPlasticity TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grossPlasticity :: TreacheryCard GrossPlasticity
grossPlasticity = treachery GrossPlasticity Cards.grossPlasticity

instance RunMessage GrossPlasticity where
  runMessage msg t@(GrossPlasticity attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mlid <- getLocationOf iid
      canFlood <- maybe (pure False) (`matches` CanHaveFloodLevelIncreased) mlid
      chooseOneM iid $ withI18n do
        countVar 1
          $ labeled' "placeAgendaDoomCanAdvance"
          $ placeDoomOnAgendaAndCheckAdvance 1
        labeledValidate' canFlood "increaseFloodLevelOfYourLocation" $ for_ mlid increaseFloodLevel
        labeled' "eachInvestigatorTakesDamageOrHorror" do
          eachInvestigator \iid' -> assignDamageOrHorror iid' attrs 1 1
      pure t
    _ -> GrossPlasticity <$> liftRunMessage msg attrs
