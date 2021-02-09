module Arkham.Types.Agenda.Cards.TheBeastUnleashed where


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
