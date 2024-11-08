module Arkham.Agenda.Cards.TheInitiationV1 (TheInitiationV1 (..), theInitiationV1) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Cultist, Suspect, ThirdFloor))

newtype TheInitiationV1 = TheInitiationV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInitiationV1 :: AgendaCard TheInitiationV1
theInitiationV1 = agenda (1, A) TheInitiationV1 Cards.theInitiationV1 (Static 6)

instance HasAbilities TheInitiationV1 where
  getAbilities (TheInitiationV1 a) = [needsAir a 1]

instance RunMessage TheInitiationV1 where
  runMessage msg a@(TheInitiationV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      n <- min 2 <$> getRemainingCurseTokens
      replicateM_ n $ addChaosToken #curse

      jailbroke <- hasMemory AJailbreak

      if jailbroke
        then do
          getSetAsideCardsMatching (CardWithTrait Suspect) >>= traverse_ \suspectCard -> do
            location <-
              maybe (selectJust $ locationIs Locations.grandEntryway) pure
                =<< selectOne (LocationWithTrait ThirdFloor)

            suspect <- createEnemyAt suspectCard location
            placeKey suspect BlueKey
        else do
          lead <- getLead
          findAndDrawEncounterCardFromEncounterDeck lead (CardWithTrait Cultist)

      advanceAgendaDeck attrs
      pure a
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      createEnemyAtLocationMatching_ ec (ConnectedTo $ locationWithInvestigator iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> TheInitiationV1 <$> liftRunMessage msg attrs
