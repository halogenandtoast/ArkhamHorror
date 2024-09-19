module Arkham.Location.Cards.TheHouseOnWaterStreet (theHouseOnWaterStreet, TheHouseOnWaterStreet (..)) where

import Arkham.Ability
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers

newtype TheHouseOnWaterStreet = TheHouseOnWaterStreet LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseOnWaterStreet :: LocationCard TheHouseOnWaterStreet
theHouseOnWaterStreet = location TheHouseOnWaterStreet Cards.theHouseOnWaterStreet 1 (PerPlayer 2)

instance HasAbilities TheHouseOnWaterStreet where
  getAbilities (TheHouseOnWaterStreet a) =
    extendRevealed1 a
      $ forcedAbility a 1
      $ SkillTestResult #after You (whileInvestigating a) (SuccessResult $ atLeast 3)

instance RunMessage TheHouseOnWaterStreet where
  runMessage msg l@(TheHouseOnWaterStreet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- take 1 . unDeck <$> getEncounterDeck
      chooseOneM iid do
        withI18n $ labeledI18n "drawTopCardOfEncounterDeck" $ drawEncounterCard iid (attrs.ability 1)
        scenarioI18n $ labeledI18n "shuffleThatCardIntoLeadsDeck" $ shuffleIntoLeadsDeck cards
      pure l
    _ -> TheHouseOnWaterStreet <$> liftRunMessage msg attrs
