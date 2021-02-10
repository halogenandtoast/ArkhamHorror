module Arkham.Types.Treachery.Cards.HuntedDown
  ( HuntedDown(..)
  , huntedDown
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


import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype HuntedDown = HuntedDown TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntedDown :: TreacheryId -> a -> HuntedDown
huntedDown uuid _ = HuntedDown $ baseAttrs uuid "01162"

instance HasModifiersFor env HuntedDown where
  getModifiersFor = noModifiersFor

instance HasActions env HuntedDown where
  getActions i window (HuntedDown attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env HuntedDown where
  runMessage msg t@(HuntedDown attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      destinationId <- getId @LocationId iid
      unengagedEnemyIds <- mapSet unUnengagedEnemyId <$> getSet ()
      criminalEnemyIds <- getSet Criminal
      let enemiesToMove = criminalEnemyIds `intersection` unengagedEnemyIds
      if null enemiesToMove
        then
          t <$ unshiftMessages
            [Surge iid $ toSource attrs, Discard $ toTarget attrs]
        else do
          messages <- for (setToList enemiesToMove) $ \eid -> do
            locationId <- getId eid
            closestLocationIds <- map unClosestPathLocationId
              <$> getSetList (locationId, destinationId)
            case closestLocationIds of
              [] -> pure Nothing
              [x] -> pure $ Just $ TargetLabel
                (EnemyTarget eid)
                ([ EnemyMove eid locationId x | locationId /= x ]
                <> [EnemyAttackIfEngaged eid (Just iid)]
                )
              xs -> pure $ Just $ TargetLabel
                (EnemyTarget eid)
                [ chooseOne
                  iid
                  [ EnemyMove eid locationId x | x <- xs, x /= locationId ]
                , EnemyAttackIfEngaged eid (Just iid)
                ]

          t <$ unless
            (null enemiesToMove || null (catMaybes messages))
            (unshiftMessages
              [ chooseOneAtATime iid (catMaybes messages)
              , Discard $ toTarget attrs
              ]
            )
    _ -> HuntedDown <$> runMessage msg attrs
