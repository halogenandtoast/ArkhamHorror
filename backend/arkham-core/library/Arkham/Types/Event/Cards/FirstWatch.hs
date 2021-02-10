module Arkham.Types.Event.Cards.FirstWatch
  ( firstWatch
  , FirstWatch(..)
  ) where

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


import Arkham.Types.Event.Attrs
import Arkham.Types.Game.Helpers

newtype FirstWatchMetadata = FirstWatchMetadata { firstWatchPairings :: [(InvestigatorId, EncounterCard)] }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype FirstWatch = FirstWatch (EventAttrs `With` FirstWatchMetadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstWatch :: InvestigatorId -> EventId -> FirstWatch
firstWatch iid uuid =
  FirstWatch $ baseAttrs iid uuid "06110" `with` FirstWatchMetadata []

instance HasActions env FirstWatch where
  getActions iid window (FirstWatch (attrs `With` _)) =
    getActions iid window attrs

instance HasModifiersFor env FirstWatch where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasSet InvestigatorId env (), HasCount PlayerCount env ()) => RunMessage env FirstWatch where
  runMessage msg e@(FirstWatch (attrs@EventAttrs {..} `With` metadata@FirstWatchMetadata {..}))
    = case msg of
      InvestigatorPlayEvent _ eid _ | eid == eventId -> do
        withQueue_ $ \(dropped : rest) -> case dropped of
          AllDrawEncounterCard -> rest
          _ -> error "AllDrawEncounterCard expected"
        playerCount <- getPlayerCount
        e <$ unshiftMessages
          [ DrawEncounterCards (EventTarget eventId) playerCount
          , Discard (toTarget attrs)
          ]
      UseCardAbility iid (EventSource eid) (Just (TargetMetadata (EncounterCardTarget card))) 1 _
        | eid == eventId
        -> do
          investigatorIds <- getSet @InvestigatorId ()
          let
            assignedInvestigatorIds = setFromList $ map fst firstWatchPairings
            remainingInvestigatorIds =
              setToList
                . insertSet iid
                $ investigatorIds
                `difference` assignedInvestigatorIds
          e <$ unshiftMessage
            (chooseOne
              iid
              [ TargetLabel
                  (InvestigatorTarget iid')
                  [ UseCardAbility
                      iid'
                      (EventSource eid)
                      (Just (TargetMetadata (EncounterCardTarget card)))
                      2
                      NoPayment
                  ]
              | iid' <- remainingInvestigatorIds
              ]
            )
      UseCardAbility iid (EventSource eid) (Just (TargetMetadata (EncounterCardTarget card))) 2 _
        | eid == eventId
        -> pure $ FirstWatch
          (attrs `with` FirstWatchMetadata
            { firstWatchPairings = (iid, card) : firstWatchPairings
            }
          )
      UseCardAbility _ (EventSource eid) Nothing 3 _ | eid == eventId ->
        e <$ unshiftMessages
          [ InvestigatorDrewEncounterCard iid' card
          | (iid', card) <- firstWatchPairings
          ]
      RequestedEncounterCards (EventTarget eid) cards | eid == eventId ->
        e <$ unshiftMessages
          [ chooseOneAtATime
            eventOwner
            [ TargetLabel
                (EncounterCardTarget card)
                [ UseCardAbility
                    eventOwner
                    (EventSource eventId)
                    (Just (TargetMetadata (EncounterCardTarget card)))
                    1
                    NoPayment
                ]
            | card <- cards
            ]
          , UseCardAbility eventOwner (EventSource eventId) Nothing 3 NoPayment
          ]
      _ -> FirstWatch . (`with` metadata) <$> runMessage msg attrs
