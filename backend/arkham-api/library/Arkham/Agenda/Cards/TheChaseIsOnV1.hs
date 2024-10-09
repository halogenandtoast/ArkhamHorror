module Arkham.Agenda.Cards.TheChaseIsOnV1 (TheChaseIsOnV1 (..), theChaseIsOnV1) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Investigator.Projection
import Arkham.Matcher.Window
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype TheChaseIsOnV1 = TheChaseIsOnV1 AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChaseIsOnV1 :: AgendaCard TheChaseIsOnV1
theChaseIsOnV1 = agenda (1, A) TheChaseIsOnV1 Cards.theChaseIsOnV1 (Static 8)

instance HasModifiersFor TheChaseIsOnV1 where
  getModifiersFor (InvestigatorTarget iid) (TheChaseIsOnV1 a) = do
    field InvestigatorPlacement iid >>= \case
      InVehicle _ -> pure []
      _ -> modified a [AdditionalActionCostOf #move 2]
  getModifiersFor _ _ = pure []

instance HasAbilities TheChaseIsOnV1 where
  getAbilities (TheChaseIsOnV1 a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage TheChaseIsOnV1 where
  runMessage msg a@(TheChaseIsOnV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceRoad
      pure a
    _ -> TheChaseIsOnV1 <$> liftRunMessage msg attrs
