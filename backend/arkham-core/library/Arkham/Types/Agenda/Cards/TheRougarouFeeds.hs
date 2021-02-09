module Arkham.Types.Agenda.Cards.TheRougarouFeeds
  ( TheRougarouFeeds(..)
  , theRougarouFeeds
  )
where


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Trait

newtype TheRougarouFeeds = TheRougarouFeeds AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRougarouFeeds :: TheRougarouFeeds
theRougarouFeeds = TheRougarouFeeds
  $ baseAttrs "81003" "The Rougarou Feeds" (Agenda 2 A) (Static 6)

instance HasModifiersFor env TheRougarouFeeds where
  getModifiersFor = noModifiersFor

instance HasActions env TheRougarouFeeds where
  getActions i window (TheRougarouFeeds x) = getActions i window x

getRougarou
  :: (MonadReader env m, HasId (Maybe StoryEnemyId) env CardCode)
  => m (Maybe EnemyId)
getRougarou = fmap unStoryEnemyId <$> getId (CardCode "81028")

instance AgendaRunner env => RunMessage env TheRougarouFeeds where
  runMessage msg a@(TheRougarouFeeds attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
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
          nonBayouLocations <- setToList . difference locations <$> getSet
            [Bayou]
          nonBayouLocationsWithClueCounts <- sortOn snd <$> for
            nonBayouLocations
            (\lid -> (lid, ) . unClueCount <$> getCount lid)
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
