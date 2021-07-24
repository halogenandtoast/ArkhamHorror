{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.Event where

import Arkham.Prelude

import Arkham.Types.Asset.Uses (UseType)
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Cards
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.Source
import Arkham.Types.Trait

$(buildEntity "Event")

createEvent :: IsCard a => a -> InvestigatorId -> Event
createEvent a iid = lookupEvent (toCardCode a) iid (EventId $ toCardId a)

instance HasCardDef Event where
  toCardDef = toCardDef . toAttrs

deriving anyclass instance
  ( HasCount ActionTakenCount env InvestigatorId
  , GetCardDef env EnemyId
  , HasActions env ActionType
  , HasSet Trait env EnemyId
  , HasSet AssetId env (InvestigatorId, UseType)
  , HasId LocationId env InvestigatorId
  , HasSet EnemyId env LocationId
  , HasSet EnemyId env InvestigatorId
  , HasSkillTest env
  )
  => HasActions env Event

instance HasCount ClueCount env InvestigatorId => HasModifiersFor env Event where
  getModifiersFor = genericGetModifiersFor

instance
  ( EventRunner env
  , HasSet FightableEnemyId env (InvestigatorId, Source)
  , HasCount HealthDamageCount env EnemyId
  , HasCount SanityDamageCount env EnemyId
  , HasCount DoomCount env AssetId
  , HasCount DoomCount env InvestigatorId
  , HasCount Shroud env LocationId
  , HasList DiscardedPlayerCard env InvestigatorId
  , HasCount FightCount env EnemyId
  )
  => RunMessage env Event where
  runMessage = genericRunMessage

instance Entity Event where
  type EntityId Event = EventId
  type EntityAttrs Event = EventAttrs

instance Named Event where
  toName = toName . toAttrs

instance TargetEntity Event where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Event where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Event where
  toCardId = toCardId . toAttrs

getEventId :: Event -> EventId
getEventId = eventId . toAttrs

lookupEvent :: CardCode -> (InvestigatorId -> EventId -> Event)
lookupEvent cardCode =
  fromJustNote ("Unknown event: " <> show cardCode) $ lookup cardCode allEvents

allEvents :: HashMap CardCode (InvestigatorId -> EventId -> Event)
allEvents = mapFromList $ map
  (cbCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Event")

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . toAttrs
