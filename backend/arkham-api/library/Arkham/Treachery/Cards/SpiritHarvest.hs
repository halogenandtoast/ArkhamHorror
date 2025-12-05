module Arkham.Treachery.Cards.SpiritHarvest (spiritHarvest) where

import Arkham.Campaigns.TheScarletKeys.Helpers (shift)
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ShadesOfSuffering.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SpiritHarvest = SpiritHarvest TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritHarvest :: TreacheryCard SpiritHarvest
spiritHarvest = treachery SpiritHarvest Cards.spiritHarvest

instance RunMessage SpiritHarvest where
  runMessage msg t@(SpiritHarvest attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      theShadeReaper <- selectJust $ scarletKeyIs Keys.theShadeReaper
      chooseOneM iid $ scenarioI18n do
        labeled' "spiritHarvest.damage" do
          eachInvestigator \iid' -> assignDamageAndHorror iid' attrs 1 1
        labeled' "spiritHarvest.tzuSanNiang" do
          shift theShadeReaper
          placeTokens attrs theShadeReaper #charge 3
      pure t
    _ -> SpiritHarvest <$> liftRunMessage msg attrs
