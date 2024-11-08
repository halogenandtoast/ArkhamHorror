module Arkham.Agenda.Cards.TheInitiationV2 (TheInitiationV2 (..), theInitiationV2) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Enemy.Creation
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Suspect, ThirdFloor))

newtype TheInitiationV2 = TheInitiationV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInitiationV2 :: AgendaCard TheInitiationV2
theInitiationV2 = agenda (1, A) TheInitiationV2 Cards.theInitiationV2 (Static 6)

instance HasAbilities TheInitiationV2 where
  getAbilities (TheInitiationV2 a) = [needsAir a 1]

instance RunMessage TheInitiationV2 where
  runMessage msg a@(TheInitiationV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      tokens <- take 2 <$> select (ChaosTokenFaceIs #curse)
      push $ ReturnChaosTokensToPool tokens
      whenRecoveredMemory AJailbreak do
        getSetAsideCardsMatching (CardWithTrait Suspect) >>= traverse_ \suspectCard -> do
          location <-
            maybe (selectJust $ locationIs Locations.grandEntryway) pure
              =<< selectOne (LocationWithTrait ThirdFloor)
          createEnemyWith suspectCard location createExhausted
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> TheInitiationV2 <$> liftRunMessage msg attrs
