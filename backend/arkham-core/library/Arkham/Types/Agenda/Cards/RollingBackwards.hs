module Arkham.Types.Agenda.Cards.RollingBackwards
  ( RollingBackwards(..)
  , rollingBackwards
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner

newtype RollingBackwards = RollingBackwards AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
  runMessage msg a@(RollingBackwards attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      investigatorIds <- getInvestigatorIds
      locationId <- getId @LocationId leadInvestigatorId
      lid <- leftmostLocation locationId
      rlid <- fromJustNote "missing right" <$> getId (RightOf, lid)
      a <$ unshiftMessages
        (RemoveLocation lid
        : RemoveLocation rlid
        : [ InvestigatorDiscardAllClues iid | iid <- investigatorIds ]
        <> [NextAgenda agendaId "02163"]
        )
    _ -> RollingBackwards <$> runMessage msg attrs
