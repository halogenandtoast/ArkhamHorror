module Arkham.Treachery.Cards.ToilAndTrouble (toilAndTrouble) where

import Arkham.Card
import Arkham.Helpers.Location
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Trait (Trait (Power))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ToilAndTrouble = ToilAndTrouble TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toilAndTrouble :: TreacheryCard ToilAndTrouble
toilAndTrouble = treachery ToilAndTrouble Cards.toilAndTrouble

instance RunMessage ToilAndTrouble where
  runMessage msg t@(ToilAndTrouble attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \location -> do
        mPowerTreachery <- find (`cardMatch` CardWithTrait Power) <$> scenarioField ScenarioDiscard
        chooseOrRunOneM iid $ scenarioI18n do
          for_ mPowerTreachery $ labeled' "toilAndTrouble.power" . drawCard iid
          labeled' "toilAndTrouble.incursion" $ resolveIncursion location
      pure t
    _ -> ToilAndTrouble <$> liftRunMessage msg attrs
