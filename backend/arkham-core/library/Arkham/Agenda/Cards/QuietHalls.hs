module Arkham.Agenda.Cards.QuietHalls where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Attrs
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Message
import Arkham.Query
import Arkham.ScenarioId

newtype QuietHalls = QuietHalls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls :: AgendaCard QuietHalls
quietHalls = agenda (1, A) QuietHalls Cards.quietHalls (Static 7)

instance AgendaRunner env => RunMessage env QuietHalls where
  runMessage msg a@(QuietHalls attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      investigatorIds <- getInvestigatorIds
      messages <- flip mapMaybeM investigatorIds $ \iid -> do
        discardCount <- unDiscardCount <$> getCount iid
        if discardCount >= 5
          then pure $ Just
            (InvestigatorAssignDamage
              iid
              (toSource attrs)
              DamageAny
              0
              (if discardCount >= 10 then 2 else 1)
            )
          else pure Nothing

      pushAll messages

      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()

      completedTheHouseAlwaysWins <-
        elem "02062" . map unCompletedScenarioId <$> getSetList ()

      let
        continueMessages = if completedTheHouseAlwaysWins
          then
            [ AdvanceAgendaDeck agendaDeckId (toSource attrs)
            , AdvanceCurrentAgenda
            ]
          else [AdvanceAgendaDeck agendaDeckId (toSource attrs)]

      a <$ push
        (chooseOne leadInvestigatorId [Label "Continue" continueMessages])
    _ -> QuietHalls <$> runMessage msg attrs
