module Arkham.Types.Event.Cards.FirstWatch
  ( firstWatch
  , FirstWatch(..)
  )
where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Game.Helpers

newtype FirstWatchMetadata = FirstWatchMetadata { firstWatchPairings :: [(InvestigatorId, EncounterCard)] }
  deriving newtype (Show, ToJSON, FromJSON)

newtype FirstWatch = FirstWatch (Attrs `With` FirstWatchMetadata)
  deriving newtype (Show, ToJSON, FromJSON)

firstWatch :: InvestigatorId -> EventId -> FirstWatch
firstWatch iid uuid =
  FirstWatch $ baseAttrs iid uuid "06110" `with` FirstWatchMetadata []

instance HasActions env FirstWatch where
  getActions iid window (FirstWatch (attrs `With` _)) =
    getActions iid window attrs

instance HasModifiersFor env FirstWatch where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasSet InvestigatorId env (), HasCount PlayerCount env ()) => RunMessage env FirstWatch where
  runMessage msg e@(FirstWatch (attrs@Attrs {..} `With` metadata@FirstWatchMetadata {..}))
    = case msg of
      InvestigatorPlayEvent _ eid _ | eid == eventId -> do
        withQueue $ \queue -> do
          let (dropped : rest) = queue
          case dropped of
            AllDrawEncounterCard -> (rest, ())
            _ -> error "AllDrawEncounterCard expected"
        playerCount <- getPlayerCount
        e <$ unshiftMessages
          [ DrawEncounterCards (EventTarget eventId) playerCount
          , Discard (toTarget attrs)
          ]
      UseCardAbility iid (EventSource eid) (Just (TargetMetadata (EncounterCardTarget card))) 1
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
                  ]
              | iid' <- remainingInvestigatorIds
              ]
            )
      UseCardAbility iid (EventSource eid) (Just (TargetMetadata (EncounterCardTarget card))) 2
        | eid == eventId
        -> pure $ FirstWatch
          (attrs `with` FirstWatchMetadata
            { firstWatchPairings = (iid, card) : firstWatchPairings
            }
          )
      UseCardAbility _ (EventSource eid) Nothing 3 | eid == eventId ->
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
                ]
            | card <- cards
            ]
          , UseCardAbility eventOwner (EventSource eventId) Nothing 3
          ]
      _ -> FirstWatch . (`with` metadata) <$> runMessage msg attrs
