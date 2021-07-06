module Arkham.Types.Agenda.Cards.TheEndOfAllThings
  ( TheEndOfAllThings
  , theEndOfAllThings
  )
where

import Arkham.Prelude

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EnemyMatcher
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Resolution

newtype TheEndOfAllThings = TheEndOfAllThings AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEndOfAllThings :: TheEndOfAllThings
theEndOfAllThings = TheEndOfAllThings
  $ baseAttrs "02315" "The End of All Things" (Agenda 4 A) (Static 2)

instance HasModifiersFor env TheEndOfAllThings where
  getModifiersFor = noModifiersFor

instance HasActions env TheEndOfAllThings where
  getActions i window (TheEndOfAllThings x) = getActions i window x

instance (HasId (Maybe EnemyId) env EnemyMatcher, AgendaRunner env) => RunMessage env TheEndOfAllThings where
  runMessage msg a@(TheEndOfAllThings attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      investigatorIds <- getInvestigatorIds
      yogSothoth <- fromJustNote "defeated?"
        <$> getId (EnemyWithTitle "Yog-Sothoth")
      a <$ pushAll
        ([ EnemyAttack iid yogSothoth | iid <- investigatorIds ]
        <> [RevertAgenda aid]
        )
    EnemyDefeated _ _ _ "02323" _ _ ->
      a <$ push (ScenarioResolution $ Resolution 3)
    _ -> TheEndOfAllThings <$> runMessage msg attrs
