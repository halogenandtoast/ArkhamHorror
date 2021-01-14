module Arkham.Types.Agenda.Cards.ATearInReality
  ( ATearInReality(..)
  , aTearInReality
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner

newtype ATearInReality = ATearInReality Attrs
  deriving newtype (Show, ToJSON, FromJSON)

aTearInReality :: ATearInReality
aTearInReality =
  ATearInReality $ baseAttrs "02160" "A Tear in Reality" (Agenda 1 A) (Static 4)

instance HasModifiersFor env ATearInReality where
  getModifiersFor = noModifiersFor

instance HasActions env ATearInReality where
  getActions i window (ATearInReality x) = getActions i window x

leftMostLocation
  :: (MonadReader env m, HasId (Maybe LocationId) env (Direction, LocationId))
  => LocationId
  -> m LocationId
leftMostLocation lid = do
  mlid' <- getId (LeftOf, lid)
  maybe (pure lid) leftMostLocation mlid'

instance AgendaRunner env => RunMessage env ATearInReality where
  runMessage msg (ATearInReality attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 A -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      investigatorIds <- getInvestigatorIds
      locationId <- getId @LocationId leadInvestigatorId
      lid <- leftMostLocation locationId
      unshiftMessages
        $ RemoveLocation lid
        : [ InvestigatorDiscardAllClues iid | iid <- investigatorIds ]
      pure $ ATearInReality $ attrs & sequenceL .~ Agenda 1 B & flippedL .~ True
    _ -> ATearInReality <$> runMessage msg attrs
