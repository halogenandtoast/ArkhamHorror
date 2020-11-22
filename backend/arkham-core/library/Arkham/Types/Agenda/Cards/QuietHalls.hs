{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.QuietHalls where

import Arkham.Import hiding (sequence)

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Game.Helpers
import Control.Monad.Extra (mapMaybeM)

newtype QuietHalls = QuietHalls Attrs
  deriving newtype (Show, ToJSON, FromJSON)

quietHalls :: QuietHalls
quietHalls =
  QuietHalls $ baseAttrs "02042" 1 "Quiet Halls" "Agenda 1a" (Static 7)

instance HasModifiersFor env QuietHalls where
  getModifiersFor = noModifiersFor

instance HasActions env QuietHalls where
  getActions i window (QuietHalls x) = getActions i window x

instance AgendaRunner env => RunMessage env QuietHalls where
  runMessage msg (QuietHalls attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1a" -> do
      investigatorIds <- getInvestigatorIds
      messages <- flip mapMaybeM investigatorIds $ \iid -> do
        discardCount <- unDiscardCount <$> getCount iid
        if discardCount >= 5
          then pure $ Just
            (InvestigatorAssignDamage
              iid
              (toSource attrs)
              0
              (if discardCount >= 10 then 2 else 1)
            )
          else pure Nothing

      unshiftMessages messages

      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()

      completedTheHouseAlwaysWins <-
        elem "02062" . map unCompletedScenarioId <$> getSetList ()

      let
        continueMessages = if completedTheHouseAlwaysWins
          then [NextAgenda aid "02043", AdvanceCurrentAgenda]
          else [NextAgenda aid "02043"]

      unshiftMessage
        (Ask leadInvestigatorId $ ChooseOne [Label "Continue" continueMessages])
      pure $ QuietHalls $ attrs & sequence .~ "Agenda 1b" & flipped .~ True
    _ -> QuietHalls <$> runMessage msg attrs
