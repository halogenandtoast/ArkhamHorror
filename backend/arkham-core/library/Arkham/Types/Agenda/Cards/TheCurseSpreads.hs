{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TheCurseSpreads
  ( TheCurseSpreads(..)
  , theCurseSpreads
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import qualified Arkham.Types.Agenda.Attrs as Agenda
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner

newtype TheCurseSpreads = TheCurseSpreads Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theCurseSpreads :: TheCurseSpreads
theCurseSpreads = TheCurseSpreads
  $ baseAttrs "81004" 3 "The Curse Spreads" "Agenda 3a" (Static 8)

instance HasActions env TheCurseSpreads where
  getActions i window (TheCurseSpreads x) = getActions i window x

getRougarou
  :: (MonadReader env m, HasId (Maybe StoryEnemyId) env CardCode)
  => m (Maybe EnemyId)
getRougarou = asks (fmap unStoryEnemyId <$> getId (CardCode "81028"))

instance AgendaRunner env => RunMessage env TheCurseSpreads where
  runMessage msg a@(TheCurseSpreads attrs@Attrs {..}) = case msg of
    EndInvestigation -> do
      mrougarou <- getRougarou
      case mrougarou of
        Nothing -> pure . TheCurseSpreads $ attrs & doom +~ 1
        Just eid -> do
          notEngaged <- asks $ null . getSet @InvestigatorId eid
          pure . TheCurseSpreads $ if notEngaged
            then attrs & doom +~ 1
            else attrs
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 3a" -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage $ chooseOne leadInvestigatorId [AdvanceAgenda aid]
      pure
        $ TheCurseSpreads
        $ attrs
        & Agenda.sequence
        .~ "Agenda 2b"
        & flipped
        .~ True
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 3b" ->
      a <$ unshiftMessage (Resolution 1)
    _ -> TheCurseSpreads <$> runMessage msg attrs
