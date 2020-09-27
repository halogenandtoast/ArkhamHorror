module Helpers.Matchers where

import ClassyPrelude

import Arkham.Types.Agenda
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.EnemyId
import Arkham.Types.Event
import Arkham.Types.EventId
import Arkham.Types.Game
import Arkham.Types.Investigator
import Arkham.Types.Location
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.TreacheryId
import qualified Data.List as L
import Lens.Micro
import Safe (fromJustNote)

isInDiscardOf
  :: (ToPlayerCard entity) => Game queue -> Investigator -> entity -> Bool
isInDiscardOf game investigator entity = card `elem` discard'
 where
  discard' = game ^?! investigators . ix (getId () investigator) . to discardOf
  card = asPlayerCard entity

class ToPlayerCard a where
  asPlayerCard :: a -> PlayerCard

class ToEncounterCard a where
  asEncounterCard :: a -> EncounterCard

instance ToPlayerCard PlayerCard where
  asPlayerCard = id

instance ToPlayerCard Event where
  asPlayerCard event =
    lookupPlayerCard (getCardCode event) (CardId . unEventId $ getId () event)

class Entity a where
  toTarget :: a -> Target
  updated :: Game queue -> a -> a

instance Entity Agenda where
  toTarget = AgendaTarget . getId ()
  updated g a = g ^?! agendas . ix (getId () a)

instance Entity Location where
  toTarget = LocationTarget . getId ()
  updated g a = g ^?! locations . ix (getId () a)

instance Entity Event where
  toTarget = EventTarget . getId ()
  updated g a = g ^?! events . ix (getId () a)

instance Entity Enemy where
  toTarget = EnemyTarget . getId ()
  updated g a = g ^?! enemies . ix (getId () a)

instance Entity Investigator where
  toTarget = InvestigatorTarget . getId ()
  updated g a = g ^?! investigators . ix (getId () a)


hasModifier :: (Entity a) => Game queue -> Modifier -> a -> Bool
hasModifier game modifier a = modifier `elem` modifiers
 where
  modifiers = case toTarget a of
    LocationTarget locId -> game ^. locations . ix locId . to getModifiers
    _ -> []

isAttachedTo :: (Entity a, Entity b) => Game queue -> a -> b -> Bool
isAttachedTo game x y = case toTarget x of
  LocationTarget locId -> case toTarget y of
    EventTarget eventId ->
      eventId `member` (game ^. locations . ix locId . to (getSet ()))
    _ -> False
  _ -> False

instance ToEncounterCard Enemy where
  asEncounterCard enemy = lookupEncounterCard
    (getCardCode enemy)
    (CardId . unEnemyId $ getId () enemy)

isInEncounterDiscard :: (ToEncounterCard entity) => Game queue -> entity -> Bool
isInEncounterDiscard game entity = card `elem` discard'
 where
  discard' = game ^. discard
  card = asEncounterCard entity

updatedResourceCount :: Game queue -> Investigator -> Int
updatedResourceCount game investigator =
  game ^?! investigators . ix (getId () investigator) . to resourceCount

evadedBy :: Game queue -> Investigator -> Enemy -> Bool
evadedBy game _investigator enemy =
  let enemy' = game ^?! enemies . ix (getId () enemy)
  in not (isEngaged enemy') && isExhausted enemy'

hasRemainingActions :: Game queue -> Int -> Investigator -> Bool
hasRemainingActions game n investigator =
  let investigator' = game ^?! investigators . ix (getId () investigator)
  in actionsRemaining investigator' == n

hasDamage :: (HasDamage a) => (Int, Int) -> a -> Bool
hasDamage n a = getDamage a == n

hasTrauma :: (HasTrauma a) => (Int, Int) -> a -> Bool
hasTrauma n a = getTrauma a == n

hasDoom :: (Entity a) => Game queue -> Int -> a -> Bool
hasDoom game n a = case toTarget a of
  AgendaTarget aid -> getCount aid game == DoomCount n
  _ -> error "Not implemented"

handIs :: Game queue -> [Card] -> Investigator -> Bool
handIs g cards i = not (null hand) && null (hand L.\\ cards)
  where hand = handOf (g ^?! investigators . ix (getId () i))

hasProcessedMessage :: Message -> Game [Message] -> Bool
hasProcessedMessage m g = m `elem` gameMessageHistory g

hasEnemy :: Enemy -> Location -> Bool
hasEnemy e l = getId () e `member` getSet @EnemyId () l

hasTreacheryWithMatchingCardCode
  :: (HasSet TreacheryId () a) => Game queue -> Card -> a -> Bool
hasTreacheryWithMatchingCardCode g c a = treacheryId `member` getSet () a
 where
  treacheryId =
    getId @TreacheryId ()
      $ fromJustNote "test failure"
      $ find ((== getCardCode c) . getCardCode)
      $ toList (g ^. treacheries)
