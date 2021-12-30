{-# LANGUAGE TemplateHaskell #-}

module Arkham.Event where

import Arkham.Prelude

import Arkham.Card
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Events
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Name
import Arkham.Query

$(buildEntity "Event")

createEvent :: IsCard a => a -> InvestigatorId -> Event
createEvent a iid = lookupEvent (toCardCode a) iid (EventId $ toCardId a)

instance HasCardCode Event where
  toCardCode = toCardCode . toAttrs

instance HasCardDef Event where
  toCardDef = toCardDef . toAttrs

instance HasAbilities Event where
  getAbilities = genericGetAbilities

instance
  ( HasCount ClueCount env InvestigatorId
  , Query EnemyMatcher env
  ) =>
  HasModifiersFor env Event
  where
  getModifiersFor = genericGetModifiersFor

instance EventRunner env => RunMessage env Event where
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

allEvents :: HashMap CardCode (InvestigatorId -> EventId -> Event)
allEvents =
  mapFromList $
    map
      (cbCardCode &&& (curry . cbCardBuilder))
      $(buildEntityLookupList "Event")

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . toAttrs
