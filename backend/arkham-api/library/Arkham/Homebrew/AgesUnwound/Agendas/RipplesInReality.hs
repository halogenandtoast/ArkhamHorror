module Arkham.Homebrew.AgesUnwound.Agendas.RipplesInReality (ripplesInReality) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Message.Lifted.Log (remember)

newtype RipplesInReality = RipplesInReality AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ripplesInReality :: AgendaCard RipplesInReality
ripplesInReality = agenda (1, A) RipplesInReality Cards.ripplesInReality (Static 6)

instance RunMessage RipplesInReality where
  runMessage msg a@(RipplesInReality attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      step <- getCurrentActStep
      if step == 1
        then do
          -- "If it is act 1: Each investigator loses each of their clues. Remember
          -- that "he is ready for you." Advance to act 1b."
          eachInvestigator (`loseAllClues` attrs)
          remember heIsReadyForYou
          advanceTheAct attrs
        else
          -- "If it is act 2 or 3: Spawn a copy of The Myriad Gentleman engaged with
          -- each investigator."
          eachInvestigator (`spawnMyriadCopiesEngagedWith` 1)
      advanceAgendaDeck attrs
      pure a
    _ -> RipplesInReality <$> liftRunMessage msg attrs
