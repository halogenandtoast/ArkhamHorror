module Arkham.Types.Treachery.Cards.OnTheProwl
  ( OnTheProwl(..)
  , onTheProwl
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


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner
import Arkham.Types.Trait

newtype OnTheProwl = OnTheProwl TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheProwl :: TreacheryId -> a -> OnTheProwl
onTheProwl uuid _ = OnTheProwl $ baseAttrs uuid "81034"

instance HasModifiersFor env OnTheProwl where
  getModifiersFor = noModifiersFor

instance HasActions env OnTheProwl where
  getActions i window (OnTheProwl attrs) = getActions i window attrs

bayouLocations
  :: (MonadReader env m, HasSet LocationId env [Trait])
  => m (HashSet LocationId)
bayouLocations = getSet [Bayou]

nonBayouLocations
  :: ( MonadReader env m
     , HasSet LocationId env ()
     , HasSet LocationId env [Trait]
     )
  => m (HashSet LocationId)
nonBayouLocations = difference <$> getLocationSet <*> bayouLocations

instance TreacheryRunner env => RunMessage env OnTheProwl where
  runMessage msg t@(OnTheProwl attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
      t <$ case mrougarou of
        Nothing -> unshiftMessage (Discard (TreacheryTarget treacheryId))
        Just eid -> do
          locationIds <- setToList <$> nonBayouLocations
          locationsWithClueCounts <- for locationIds
            $ \lid -> (lid, ) . unClueCount <$> getCount lid
          let
            sortedLocationsWithClueCounts = sortOn snd locationsWithClueCounts
          case sortedLocationsWithClueCounts of
            [] -> unshiftMessage (Discard (TreacheryTarget treacheryId))
            ((_, c) : _) ->
              let
                (matches, _) =
                  span ((== c) . snd) sortedLocationsWithClueCounts
              in
                case matches of
                  [(x, _)] -> unshiftMessages
                    [ MoveUntil x (EnemyTarget eid)
                    , Discard (TreacheryTarget treacheryId)
                    ]
                  xs -> unshiftMessages
                    [ chooseOne
                      iid
                      [ MoveUntil x (EnemyTarget eid) | (x, _) <- xs ]
                    , Discard (TreacheryTarget treacheryId)
                    ]
    _ -> OnTheProwl <$> runMessage msg attrs
