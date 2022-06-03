module Arkham.Agenda.Cards.UndergroundMuscle
  ( UndergroundMuscle(..)
  , undergroundMuscle
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.EncounterSet
import Arkham.Agenda.Attrs
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.EnemyId
import Arkham.GameValue
import Arkham.InvestigatorId
import Arkham.Message
import Arkham.Query
import Data.Maybe (fromJust)

newtype UndergroundMuscle = UndergroundMuscle AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundMuscle :: AgendaCard UndergroundMuscle
undergroundMuscle =
  agenda (2, A) UndergroundMuscle Cards.undergroundMuscle (Static 3)

instance AgendaRunner env => RunMessage UndergroundMuscle where
  runMessage msg (UndergroundMuscle attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      laBellaLunaId <- getJustLocationIdByName "La Bella Luna"
      cloverClubLoungeId <- getJustLocationIdByName "Clover Club Lounge"
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      result <- shuffleM =<< gatherEncounterSet HideousAbominations
      let
        enemy = fromJust . headMay $ result
        rest = drop 1 result
      strikingFear <- gatherEncounterSet StrikingFear
      laBellaLunaInvestigators <- getSetList laBellaLunaId
      laBellaLunaEnemies <- getSetList @EnemyId laBellaLunaId
      unEngagedEnemiesAtLaBellaLuna <- filterM
        (\eid -> null <$> getSetList @InvestigatorId eid)
        laBellaLunaEnemies
      push
        (chooseOne
          leadInvestigatorId
          [ Label
              "Continue"
              ([ CreateEnemyAt (EncounterCard enemy) cloverClubLoungeId Nothing
               , ShuffleEncounterDiscardBackIn
               , ShuffleIntoEncounterDeck $ rest <> strikingFear
               ]
              <> [ MoveAction iid cloverClubLoungeId Free False
                 | iid <- laBellaLunaInvestigators
                 ]
              <> [ EnemyMove eid cloverClubLoungeId
                 | eid <- unEngagedEnemiesAtLaBellaLuna
                 ]
              <> [ RemoveLocation laBellaLunaId
                 , AdvanceAgendaDeck agendaDeckId (toSource attrs)
                 ]
              )
          ]
        )
      pure
        $ UndergroundMuscle
        $ attrs
        & (sequenceL .~ Agenda 1 B)
        & (flippedL .~ True)
    _ -> UndergroundMuscle <$> runMessage msg attrs
