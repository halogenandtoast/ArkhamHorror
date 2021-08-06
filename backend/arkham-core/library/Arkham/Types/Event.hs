{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.Event where

import Arkham.Prelude

import Arkham.Card
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Cards
import Arkham.Types.Event.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.Source

$(buildEntity "Event")

createEvent :: IsCard a => a -> InvestigatorId -> Event
createEvent a iid = lookupEvent (toCardCode a) iid (EventId $ toCardId a)

instance HasCardCode Event where
  toCardCode = toCardCode . toAttrs

instance HasCardDef Event where
  toCardDef = toCardDef . toAttrs

deriving anyclass instance
  ( CanCheckPlayable env
  , GetCardDef env EnemyId
  , HasSkillTest env
  )
  => HasActions env Event

instance HasCount ClueCount env InvestigatorId => HasModifiersFor env Event where
  getModifiersFor = genericGetModifiersFor

instance
  ( EventRunner env
  , Query InvestigatorMatcher env
  , HasSet FightableEnemyId env (InvestigatorId, Source)
  , HasCount HealthDamageCount env EnemyId
  , HasCount SanityDamageCount env EnemyId
  , HasCount Shroud env LocationId
  , HasCount FightCount env EnemyId
  , CanCheckPlayable env
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
  toCard e = lookupCard (eventOriginalCardCode . toAttrs $ e) (toCardId e)

instance HasId InvestigatorId env Event where
  getId = pure . eventOwner . toAttrs

instance HasId OwnerId env Event where
  getId = fmap OwnerId . getId

getEventId :: Event -> EventId
getEventId = eventId . toAttrs

lookupEvent :: CardCode -> (InvestigatorId -> EventId -> Event)
lookupEvent cardCode =
  fromJustNote ("Unknown event: " <> show cardCode) $ lookup cardCode allEvents

allEvents :: Map CardCode (InvestigatorId -> EventId -> Event)
allEvents = mapFromList $ map
  (cbCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Event")

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . toAttrs
