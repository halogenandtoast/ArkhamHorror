module Arkham.Types.Agenda.Cards.QuietHalls where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.ScenarioId
import Control.Monad.Extra (mapMaybeM)

newtype QuietHalls = QuietHalls AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls :: AgendaCard QuietHalls
quietHalls = agenda (1, A) QuietHalls Cards.quietHalls (Static 7)

instance HasModifiersFor env QuietHalls
instance HasActions QuietHalls

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
          then [NextAgenda aid "02043", AdvanceCurrentAgenda]
          else [NextAgenda aid "02043"]

      a <$ push
        (chooseOne leadInvestigatorId [Label "Continue" continueMessages])
    _ -> QuietHalls <$> runMessage msg attrs
