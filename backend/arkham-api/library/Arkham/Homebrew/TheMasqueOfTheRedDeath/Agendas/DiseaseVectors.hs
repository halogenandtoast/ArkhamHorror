module Arkham.Homebrew.TheMasqueOfTheRedDeath.Agendas.DiseaseVectors (diseaseVectors) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (addSkullEffectsToToken)

newtype DiseaseVectors = DiseaseVectors AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diseaseVectors :: AgendaCard DiseaseVectors
diseaseVectors = agenda (3, A) DiseaseVectors Cards.diseaseVectors (Static 6)

instance RunMessage DiseaseVectors where
  runMessage msg a@(DiseaseVectors attrs) = runQueueT $ case msg of
    -- "Add each [skull] effect on your location to each [cultist], [tablet], and
    -- [elder_thing] token you reveal during skill tests."
    ResolveChaosToken token face iid
      | onSide A attrs
      , face `elem` [#cultist, #tablet, #elderthing] -> do
          addSkullEffectsToToken iid token
          pure a
    -- "Each investigator who has not resigned is killed."
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator (kill attrs)
      pure a
    _ -> DiseaseVectors <$> liftRunMessage msg attrs
