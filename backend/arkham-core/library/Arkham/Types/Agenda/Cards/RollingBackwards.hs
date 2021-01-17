module Arkham.Types.Agenda.Cards.RollingBackwards
  ( RollingBackwards(..)
  , rollingBackwards
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner

newtype RollingBackwards = RollingBackwards Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rollingBackwards :: RollingBackwards
rollingBackwards = RollingBackwards
  $ baseAttrs "02162" "Rolling Backwards" (Agenda 3 A) (Static 4)

instance HasModifiersFor env RollingBackwards where
  getModifiersFor = noModifiersFor

instance HasActions env RollingBackwards where
  getActions i window (RollingBackwards x) = getActions i window x

leftmostLocation
  :: (MonadReader env m, HasId (Maybe LocationId) env (Direction, LocationId))
  => LocationId
  -> m LocationId
leftmostLocation lid = do
  mlid' <- getId (LeftOf, lid)
  maybe (pure lid) leftmostLocation mlid'

instance AgendaRunner env => RunMessage env RollingBackwards where
  runMessage msg (RollingBackwards attrs@Attrs {..}) = case msg of
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
        <> [NextAgenda agendaId "02163"]
      pure
        $ RollingBackwards
        $ attrs
        & sequenceL
        .~ Agenda 3 B
        & flippedL
        .~ True
    _ -> RollingBackwards <$> runMessage msg attrs
