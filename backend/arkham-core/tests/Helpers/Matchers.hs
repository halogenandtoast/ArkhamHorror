module Helpers.Matchers where

import ClassyPrelude

import Arkham.Types.Agenda
import Arkham.Types.Asset
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard (playerCardAttrs)
import Arkham.Types.Card.PlayerCard.Attrs (pcCardType)
import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.EnemyId
import Arkham.Types.Event
import Arkham.Types.EventId
import Arkham.Types.Game
import Arkham.Types.Investigator
import Arkham.Types.Location
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Treachery
import Arkham.Types.TreacheryId
import qualified Data.List as L
import Lens.Micro

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

instance ToPlayerCard Treachery where
  asPlayerCard treachery = lookupPlayerCard
    (getCardCode treachery)
    (CardId . unTreacheryId $ getId () treachery)

class Entity a where
  toTarget :: a -> Target
  updated :: Game queue -> a -> a

instance Entity Agenda where
  toTarget = AgendaTarget . getId ()
  updated g a = g ^?! agendas . ix (getId () a)

instance Entity Treachery where
  toTarget = TreacheryTarget . getId ()
  updated g t = g ^?! treacheries . ix (getId () t)

instance Entity Asset where
  toTarget = AssetTarget . getId ()
  updated g a = g ^?! assets . ix (getId () a)

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

handIs :: [Card] -> Investigator -> Bool
handIs cards i = flip handMatches i $ \hand ->
  null (foldr (flip (L.\\) . pure) hand cards) && length cards == length hand

handMatches :: ([Card] -> Bool) -> Investigator -> Bool
handMatches f i = f (handOf i)

deckMatches :: ([PlayerCard] -> Bool) -> Investigator -> Bool
deckMatches f i = f (deckOf i)

hasProcessedMessage :: Message -> Game [Message] -> Bool
hasProcessedMessage m g = m `elem` gameMessageHistory g

hasEnemy :: Enemy -> Location -> Bool
hasEnemy e l = getId () e `member` getSet @EnemyId () l

hasCardInPlay :: Card -> Investigator -> Bool
hasCardInPlay c i = case c of
  PlayerCard pc -> case pcCardType (playerCardAttrs pc) of
    AssetType -> AssetId (unCardId $ getCardId pc) `member` getSet () i
    _ -> error "not implemented"
  _ -> error "not implemented"

hasTreacheryWithMatchingCardCode
  :: (HasSet TreacheryId () a) => Game queue -> Card -> a -> Bool
hasTreacheryWithMatchingCardCode g c a = maybe
  False
  (\treachery -> getId @TreacheryId () treachery `member` getSet () a)
  mtreachery
 where
  mtreachery =
    find ((== getCardCode c) . getCardCode) $ toList (g ^. treacheries)

hasClueCount :: HasCount ClueCount () a => Int -> a -> Bool
hasClueCount n a = n == unClueCount (getCount () a)

hasUses :: (HasCount UsesCount () a) => Int -> a -> Bool
hasUses n a = n == unUsesCount (getCount () a)
