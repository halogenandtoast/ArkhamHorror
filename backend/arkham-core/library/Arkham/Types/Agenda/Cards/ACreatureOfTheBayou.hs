module Arkham.Types.Agenda.Cards.ACreatureOfTheBayou
  ( ACreatureOfTheBayou(..)
  , aCreatureOfTheBayou
  )
where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


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
