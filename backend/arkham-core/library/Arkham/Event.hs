{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Event where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Events
import Arkham.Event.Runner
import Arkham.Id

createEvent :: IsCard a => a -> InvestigatorId -> Event
createEvent a iid = lookupEvent (toCardCode a) iid (EventId $ toCardId a)

instance RunMessage Event where
  runMessage msg (Event a) = Event <$> runMessage msg a

lookupEvent :: CardCode -> (InvestigatorId -> EventId -> Event)
lookupEvent cardCode = case lookup cardCode allEvents of
  Nothing -> error $ "Unknown event: " <> show cardCode
  Just (SomeEventCard a) -> \i e -> Event $ cbCardBuilder a (i, e)

instance FromJSON Event where
  parseJSON v = flip (withObject "Event") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withEventCardCode cCode $ \(_ :: EventCard a) -> Event <$> parseJSON @a v

withEventCardCode
  :: CardCode
  -> (forall a. IsEvent a => EventCard a -> r)
  -> r
withEventCardCode cCode f =
  case lookup cCode allEvents of
    Nothing -> error $ "Unknown event: " <> show cCode
    Just (SomeEventCard a) -> f a

allEvents :: HashMap CardCode SomeEventCard
allEvents = mapFromList $ map
  (toFst someEventCardCode)
  [ SomeEventCard dodge
  , SomeEventCard dynamiteBlast
  , SomeEventCard evidence
  , SomeEventCard workingAHunch
  , SomeEventCard emergencyCache
  ]
