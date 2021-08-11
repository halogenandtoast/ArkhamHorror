module Arkham.Types.Agenda.Cards.TheBeastUnleashed where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Target

newtype TheBeastUnleashed = TheBeastUnleashed AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBeastUnleashed :: AgendaCard TheBeastUnleashed
theBeastUnleashed =
  agenda (3, A) TheBeastUnleashed Cards.theBeastUnleashed (Static 2)

instance HasActions TheBeastUnleashed
instance HasModifiersFor env TheBeastUnleashed

instance AgendaRunner env => RunMessage env TheBeastUnleashed where
  runMessage msg a@(TheBeastUnleashed attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgendaIfThresholdSatisfied -> do
      perPlayerDoomThreshold <- getPlayerCountValue agendaDoomThreshold
      totalDoom <- unDoomCount <$> getCount ()
      experimentId <- unStoryEnemyId . fromJustNote "must be in play" <$> getId
        (CardCode "02058")
      a <$ when
        (totalDoom >= perPlayerDoomThreshold)
        (pushAll
          [ RemoveAllDoom
          , MoveToward
            (EnemyTarget experimentId)
            (LocationWithTitle "Dormitories")
          ]
        )
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      investigatorIds <- getInvestigatorIds
      a <$ pushAll
        ([ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 3
         | iid <- investigatorIds
         ]
        <> [Label "Resolution 4" [ScenarioResolution $ Resolution 4]]
        )
    EnemyEntered eid lid -> do
      experimentId <- unStoryEnemyId . fromJustNote "must be in play" <$> getId
        (CardCode "02058")
      mDormitoriesId <- getId (LocationWithTitle "Dormitories")
      a <$ when
        (mDormitoriesId == Just lid && eid == experimentId)
        (push $ AdvanceAgenda (toId attrs))
    _ -> TheBeastUnleashed <$> runMessage msg attrs
