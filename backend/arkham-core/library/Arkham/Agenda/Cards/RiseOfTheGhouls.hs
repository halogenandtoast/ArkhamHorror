module Arkham.Agenda.Cards.RiseOfTheGhouls
  ( RiseOfTheGhouls(..)
  , riseOfTheGhouls
  ) where

import Arkham.Prelude

import Arkham.Agenda.Types
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Source
import Arkham.Trait

newtype RiseOfTheGhouls = RiseOfTheGhouls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riseOfTheGhouls :: AgendaCard RiseOfTheGhouls
riseOfTheGhouls =
  agenda (2, A) RiseOfTheGhouls Cards.riseOfTheGhouls (Static 7)

instance RunMessage RiseOfTheGhouls where
  runMessage msg a@(RiseOfTheGhouls attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> a <$ push
      (Run
        [ ShuffleEncounterDiscardBackIn
        , DiscardEncounterUntilFirst
          (AgendaSource aid)
          Nothing
          (CardWithType EnemyType <> CardWithTrait Ghoul)
        ]
      )
    RequestedEncounterCard (AgendaSource aid) _ mcard | aid == agendaId ->
      case mcard of
        Nothing -> a <$ push (AdvanceAgendaDeck agendaDeckId (toSource attrs))
        Just card -> do
          leadInvestigatorId <- getLeadInvestigatorId
          a <$ pushAll
            [ InvestigatorDrewEncounterCard leadInvestigatorId card
            , AdvanceAgendaDeck agendaDeckId (toSource attrs)
            ]
    _ -> RiseOfTheGhouls <$> runMessage msg attrs
