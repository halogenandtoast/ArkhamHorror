module Arkham.Location.Cards.InnsmouthHarbour (innsmouthHarbour, InnsmouthHarbour (..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers

newtype InnsmouthHarbour = InnsmouthHarbour LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthHarbour :: LocationCard InnsmouthHarbour
innsmouthHarbour = location InnsmouthHarbour Cards.innsmouthHarbour 3 (PerPlayer 2)

instance HasAbilities InnsmouthHarbour where
  getAbilities (InnsmouthHarbour a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restrictedAbility a 1 (ScenarioDeckWithCard LeadsDeck)
      $ forced (SkillTestResult #after You (whileInvestigating a) (SuccessResult $ atLeast 3))

instance RunMessage InnsmouthHarbour where
  runMessage msg l@(InnsmouthHarbour attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      leads <- take 1 <$> getLeadsDeck
      for_ leads crossOutLead
      focusCards leads \unfocus -> continue iid [unfocus]
      shuffleLeadsDeck
      pure l
    _ -> InnsmouthHarbour <$> liftRunMessage msg attrs
