module Arkham.Types.Agenda.Cards.DrawnIn
  ( DrawnIn(..)
  , drawnIn
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner

newtype DrawnIn = DrawnIn Attrs
  deriving newtype (Show, ToJSON, FromJSON)

drawnIn :: DrawnIn
drawnIn = DrawnIn $ baseAttrs "02163" "Drawn In" (Agenda 4 A) (Static 3)

instance HasModifiersFor env DrawnIn where
  getModifiersFor = noModifiersFor

instance HasActions env DrawnIn where
  getActions i window (DrawnIn x) = getActions i window x

leftmostLocation
  :: (MonadReader env m, HasId (Maybe LocationId) env (Direction, LocationId))
  => LocationId
  -> m LocationId
leftmostLocation lid = do
  mlid' <- getId (LeftOf, lid)
  maybe (pure lid) leftmostLocation mlid'

instance AgendaRunner env => RunMessage env DrawnIn where
  runMessage msg (DrawnIn attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 A -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      investigatorIds <- getInvestigatorIds
      locationId <- getId @LocationId leadInvestigatorId
      lid <- leftmostLocation locationId
      rlid <- fromJustNote "missing right" <$> getId (RightOf, lid)
      unshiftMessages
        $ RemoveLocation lid
        : RemoveLocation rlid
        : [ InvestigatorDiscardAllClues iid | iid <- investigatorIds ]
        <> [NextAgenda agendaId "02164"]
      pure $ DrawnIn $ attrs & sequenceL .~ Agenda 3 B & flippedL .~ True
    _ -> DrawnIn <$> runMessage msg attrs
