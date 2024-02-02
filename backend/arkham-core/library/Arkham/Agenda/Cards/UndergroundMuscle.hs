module Arkham.Agenda.Cards.UndergroundMuscle (
  UndergroundMuscle (..),
  undergroundMuscle,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection
import Data.Maybe (fromJust)

newtype UndergroundMuscle = UndergroundMuscle AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

undergroundMuscle :: AgendaCard UndergroundMuscle
undergroundMuscle =
  agenda (2, A) UndergroundMuscle Cards.undergroundMuscle (Static 3)

instance RunMessage UndergroundMuscle where
  runMessage msg (UndergroundMuscle attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      laBellaLunaId <- getJustLocationByName "La Bella Luna"
      cloverClubLoungeId <- getJustLocationByName "Clover Club Lounge"
      lead <- getLeadPlayer
      result <- shuffleM =<< gatherEncounterSet HideousAbominations
      let
        enemy = fromJust . headMay $ result
        rest = drop 1 result
      strikingFear <- gatherEncounterSet StrikingFear
      laBellaLunaInvestigators <-
        selectList
          $ InvestigatorAt
          $ LocationWithId
            laBellaLunaId
      laBellaLunaEnemies <- selectList $ EnemyAt $ LocationWithId laBellaLunaId
      unEngagedEnemiesAtLaBellaLuna <-
        filterM
          (fieldMap EnemyEngagedInvestigators null)
          laBellaLunaEnemies

      enemyCreation <-
        createEnemyAt_
          (EncounterCard enemy)
          cloverClubLoungeId
          Nothing

      push
        $ chooseOne
          lead
          [ Label "Continue"
              $ [ enemyCreation
                , ShuffleEncounterDiscardBackIn
                , ShuffleCardsIntoDeck Deck.EncounterDeck
                    $ map EncounterCard (rest <> strikingFear)
                ]
              <> [ Move $ move attrs iid cloverClubLoungeId
                 | iid <- laBellaLunaInvestigators
                 ]
              <> [ EnemyMove eid cloverClubLoungeId
                 | eid <- unEngagedEnemiesAtLaBellaLuna
                 ]
              <> [ RemoveLocation laBellaLunaId
                 , AdvanceAgendaDeck agendaDeckId (toSource attrs)
                 ]
          ]

      pure
        $ UndergroundMuscle
        $ attrs
        & (sequenceL .~ Sequence 1 B)
        & (flippedL .~ True)
    _ -> UndergroundMuscle <$> runMessage msg attrs
