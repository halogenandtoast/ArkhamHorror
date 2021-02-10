module Arkham.Types.Agenda.Cards.UndergroundMuscle
  ( UndergroundMuscle(..)
  , undergroundMuscle
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.EncounterSet

newtype UndergroundMuscle = UndergroundMuscle AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundMuscle :: UndergroundMuscle
undergroundMuscle = UndergroundMuscle
  $ baseAttrs "02064" "Underground Muscle" (Agenda 2 A) (Static 3)

instance HasActions env UndergroundMuscle where
  getActions i window (UndergroundMuscle x) = getActions i window x

instance HasModifiersFor env UndergroundMuscle where
  getModifiersFor = noModifiersFor

instance AgendaRunner env => RunMessage env UndergroundMuscle where
  runMessage msg (UndergroundMuscle attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      laBellaLunaId <- fromJustNote "La Bella Luna is missing"
        <$> getLocationIdWithTitle "La Bella Luna"
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      (enemy : rest) <- shuffleM =<< gatherEncounterSet HideousAbominations
      strikingFear <- gatherEncounterSet StrikingFear
      laBellaLunaInvestigators <- getSetList laBellaLunaId
      laBellaLunaEnemies <- getSetList @EnemyId laBellaLunaId
      unEngagedEnemiesAtLaBellaLuna <- filterM
        (\eid -> null <$> getSetList @InvestigatorId eid)
        laBellaLunaEnemies
      unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Label
              "Continue"
              ([ CreateEnemyAtLocationMatching
                 (EncounterCard enemy)
                 (LocationWithTitle "Clover Club Lounge")
               , ShuffleEncounterDiscardBackIn
               , ShuffleIntoEncounterDeck $ rest <> strikingFear
               ]
              <> [ MoveAction iid "02070" False
                 | iid <- laBellaLunaInvestigators
                 ]
              <> [ EnemyMove eid laBellaLunaId "02070"
                 | eid <- unEngagedEnemiesAtLaBellaLuna
                 ]
              <> [RemoveLocation laBellaLunaId, NextAgenda agendaId "02065"]
              )
          ]
        )
      pure
        $ UndergroundMuscle
        $ attrs
        & sequenceL
        .~ Agenda 1 B
        & flippedL
        .~ True
    _ -> UndergroundMuscle <$> runMessage msg attrs
