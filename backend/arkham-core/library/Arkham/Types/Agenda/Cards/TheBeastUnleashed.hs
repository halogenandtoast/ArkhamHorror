module Arkham.Types.Agenda.Cards.TheBeastUnleashed where

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
import Arkham.Types.Game.Helpers

newtype TheBeastUnleashed = TheBeastUnleashed AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBeastUnleashed :: TheBeastUnleashed
theBeastUnleashed = TheBeastUnleashed
  $ baseAttrs "02044" "The Beast Unleashed" (Agenda 3 A) (Static 2)

instance HasActions env TheBeastUnleashed where
  getActions i window (TheBeastUnleashed x) = getActions i window x

instance HasModifiersFor env TheBeastUnleashed where
  getModifiersFor = noModifiersFor

instance AgendaRunner env => RunMessage env TheBeastUnleashed where
  runMessage msg a@(TheBeastUnleashed attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgendaIfThresholdSatisfied -> do
      perPlayerDoomThreshold <- getPlayerCountValue agendaDoomThreshold
      totalDoom <- unDoomCount <$> getCount ()
      experimentId <- unStoryEnemyId . fromJustNote "must be in play" <$> getId
        (CardCode "02058")
      a <$ when
        (totalDoom >= perPlayerDoomThreshold)
        (unshiftMessages
          [ RemoveAllDoom
          , MoveToward
            (EnemyTarget experimentId)
            (LocationWithTitle "Dormitories")
          ]
        )
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      investigatorIds <- getInvestigatorIds
      a <$ unshiftMessages
        ([ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 3
         | iid <- investigatorIds
         ]
        <> [Label "Resolution 3" [ScenarioResolution $ Resolution 3]]
        )
    _ -> TheBeastUnleashed <$> runMessage msg attrs
