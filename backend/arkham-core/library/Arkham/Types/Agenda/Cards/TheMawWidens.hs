module Arkham.Types.Agenda.Cards.TheMawWidens
  ( TheMawWidens(..)
  , theMawWidens
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner

newtype TheMawWidens = TheMawWidens Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theMawWidens :: TheMawWidens
theMawWidens =
  TheMawWidens $ baseAttrs "02161" "The Maw Widens" (Agenda 2 A) (Static 3)

instance HasModifiersFor env TheMawWidens where
  getModifiersFor = noModifiersFor

instance HasActions env TheMawWidens where
  getActions i window (TheMawWidens x) = getActions i window x

leftMostLocation
  :: (MonadReader env m, HasId (Maybe LocationId) env (Direction, LocationId))
  => LocationId
  -> m LocationId
leftMostLocation lid = do
  mlid' <- getId (LeftOf, lid)
  maybe (pure lid) leftMostLocation mlid'

instance AgendaRunner env => RunMessage env TheMawWidens where
  runMessage msg (TheMawWidens attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 A -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      investigatorIds <- getInvestigatorIds
      locationId <- getId @LocationId leadInvestigatorId
      lid <- leftMostLocation locationId
      unshiftMessages
        $ RemoveLocation lid
        : [ InvestigatorDiscardAllClues iid | iid <- investigatorIds ]
      pure $ TheMawWidens $ attrs & sequenceL .~ Agenda 2 B & flippedL .~ True
    _ -> TheMawWidens <$> runMessage msg attrs
