{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TheRougarouFeeds
  ( TheRougarouFeeds(..)
  , theRougarouFeeds
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import qualified Arkham.Types.Agenda.Attrs as Agenda
import Arkham.Types.Agenda.Runner
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Trait

newtype TheRougarouFeeds = TheRougarouFeeds Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theRougarouFeeds :: TheRougarouFeeds
theRougarouFeeds = TheRougarouFeeds
  $ baseAttrs "81003" 2 "The Rougarou Feeds" "Agenda 2a" (Static 6)

instance HasActions env TheRougarouFeeds where
  getActions i window (TheRougarouFeeds x) = getActions i window x

getRougarou
  :: (MonadReader env m, HasId (Maybe StoryEnemyId) CardCode env)
  => m (Maybe EnemyId)
getRougarou = asks (fmap unStoryEnemyId <$> getId (CardCode "81028"))

instance AgendaRunner env => RunMessage env TheRougarouFeeds where
  runMessage msg a@(TheRougarouFeeds attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2a" -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage $ chooseOne leadInvestigatorId [AdvanceAgenda aid]
      pure
        $ TheRougarouFeeds
        $ attrs
        & Agenda.sequence
        .~ "Agenda 2b"
        & flipped
        .~ True
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2b" -> do
      mrougarou <- getRougarou
      case mrougarou of
        Nothing -> a <$ unshiftMessages
          [ ShuffleAllInEncounterDiscardBackIn "81034"
          , NextAgenda aid "81004"
          , PlaceDoomOnAgenda
          ]
        Just eid -> do
          leadInvestigatorId <- getLeadInvestigatorId
          locations <- getLocationSet
          nonBayouLocations <- asks $ setToList . difference locations . getSet
            [Bayou]
          nonBayouLocationsWithClueCounts <- sortOn snd <$> for
            nonBayouLocations
            (\lid -> asks $ (lid, ) . unClueCount . getCount lid)
          let
            moveMessage = case nonBayouLocationsWithClueCounts of
              [] -> error "there has to be such a location"
              ((_, c) : _) ->
                let
                  (matches, _) =
                    span ((== c) . snd) nonBayouLocationsWithClueCounts
                in
                  case matches of
                    [(x, _)] -> MoveUntil x (EnemyTarget eid)
                    xs -> chooseOne
                      leadInvestigatorId
                      [ MoveUntil x (EnemyTarget eid) | (x, _) <- xs ]
          a <$ unshiftMessages
            [ ShuffleAllInEncounterDiscardBackIn "81034"
            , moveMessage
            , NextAgenda aid "81004"
            ]
    _ -> TheRougarouFeeds <$> runMessage msg attrs
