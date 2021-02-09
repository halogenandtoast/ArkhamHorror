module Arkham.Types.Agenda.Cards.ACreatureOfTheBayou
  ( ACreatureOfTheBayou(..)
  , aCreatureOfTheBayou
  )
where


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Trait

newtype ACreatureOfTheBayou = ACreatureOfTheBayou AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aCreatureOfTheBayou :: ACreatureOfTheBayou
aCreatureOfTheBayou = ACreatureOfTheBayou
  $ baseAttrs "81002" "A Creature of the Bayou" (Agenda 1 A) (Static 5)

instance HasModifiersFor env ACreatureOfTheBayou where
  getModifiersFor = noModifiersFor

instance HasActions env ACreatureOfTheBayou where
  getActions i window (ACreatureOfTheBayou x) = getActions i window x

getRougarou
  :: (MonadReader env m, HasId (Maybe StoryEnemyId) env CardCode)
  => m (Maybe EnemyId)
getRougarou = fmap unStoryEnemyId <$> getId (CardCode "81028")

instance AgendaRunner env => RunMessage env ACreatureOfTheBayou where
  runMessage msg a@(ACreatureOfTheBayou attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      mrougarou <- getRougarou
      case mrougarou of
        Nothing -> a <$ unshiftMessages
          [ ShuffleEncounterDiscardBackIn
          , NextAgenda aid "81003"
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
            [ShuffleEncounterDiscardBackIn, moveMessage, NextAgenda aid "81003"]
    _ -> ACreatureOfTheBayou <$> runMessage msg attrs
