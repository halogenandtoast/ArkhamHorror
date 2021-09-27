module Arkham.Types.Agenda.Cards.ACreatureOfTheBayou
  ( ACreatureOfTheBayou(..)
  , aCreatureOfTheBayou
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Scenarios.CurseOfTheRougarou.Helpers
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target

newtype ACreatureOfTheBayou = ACreatureOfTheBayou AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aCreatureOfTheBayou :: AgendaCard ACreatureOfTheBayou
aCreatureOfTheBayou =
  agenda (1, A) ACreatureOfTheBayou Cards.aCreatureOfTheBayou (Static 5)

instance AgendaRunner env => RunMessage env ACreatureOfTheBayou where
  runMessage msg a@(ACreatureOfTheBayou attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      mrougarou <- getTheRougarou
      case mrougarou of
        Nothing -> a <$ pushAll
          [ ShuffleEncounterDiscardBackIn
          , NextAgenda aid "81003"
          , PlaceDoomOnAgenda
          ]
        Just eid -> do
          leadInvestigatorId <- getLeadInvestigatorId
          targets <- setToList <$> nonBayouLocations
          nonBayouLocationsWithClueCounts <-
            sortOn snd
              <$> traverse (traverseToSnd (fmap unClueCount . getCount)) targets
          let
            moveMessage = case nonBayouLocationsWithClueCounts of
              [] -> error "there has to be such a location"
              ((_, c) : _) ->
                let
                  (matches, _) =
                    span ((== c) . snd) nonBayouLocationsWithClueCounts
                in
                  case matches of
                    [(x, _)] -> MoveUntil x (EnemyTarget eid)
                    xs -> chooseOne
                      leadInvestigatorId
                      [ MoveUntil x (EnemyTarget eid) | (x, _) <- xs ]
          a
            <$ pushAll
                 [ ShuffleEncounterDiscardBackIn
                 , moveMessage
                 , NextAgenda aid "81003"
                 ]
    _ -> ACreatureOfTheBayou <$> runMessage msg attrs
