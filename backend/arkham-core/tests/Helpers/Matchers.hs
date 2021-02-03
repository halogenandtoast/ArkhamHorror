module Helpers.Matchers where

import Arkham.Prelude

import Arkham.Types.Agenda
import Arkham.Types.Asset
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.EnemyId
import Arkham.Types.Event
import Arkham.Types.EventId
import Arkham.Types.Game
import Arkham.Types.Investigator
import qualified Arkham.Types.Investigator.Attrs as Investigator
import Arkham.Types.Location
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Treachery
import Arkham.Types.TreacheryId
import Control.Lens
import qualified Data.List as L

isInDiscardOf
  :: (ToPlayerCard entity) => Game queue -> Investigator -> entity -> Bool
isInDiscardOf game investigator entity = card `elem` discard'
 where
  discard' = game ^?! investigatorsL . ix (toId investigator) . to discardOf
  card = asPlayerCard entity

class ToPlayerCard a where
  asPlayerCard :: a -> PlayerCard

class ToEncounterCard a where
  asEncounterCard :: a -> EncounterCard

instance ToPlayerCard PlayerCard where
  asPlayerCard = id

instance ToPlayerCard Event where
  asPlayerCard event =
    lookupPlayerCard (getCardCode event) (CardId . unEventId $ toId event)

instance ToPlayerCard Treachery where
  asPlayerCard treachery = lookupPlayerCard
    (getCardCode treachery)
    (CardId . unTreacheryId $ toId treachery)

class (Entity a, TargetEntity a) => TestEntity a where
  updated :: Game queue -> a -> a

instance TestEntity Agenda where
  updated g a = g ^?! agendasL . ix (toId a)

instance TestEntity Treachery where
  updated g t = g ^?! treacheriesL . ix (toId t)

instance TestEntity Asset where
  updated g a = g ^?! assetsL . ix (toId a)

instance TestEntity Location where
  updated g l = g ^?! locationsL . ix (toId l)

instance TestEntity Event where
  updated g e = g ^?! eventsL . ix (toId e)

instance TestEntity Enemy where
  updated g e = g ^?! enemiesL . ix (toId e)

instance TestEntity Investigator where
  updated g i = g ^?! investigatorsL . ix (toId i)

isAttachedTo :: (TestEntity a, TestEntity b) => Game queue -> a -> b -> Bool
isAttachedTo game x y = case toTarget x of
  LocationTarget locId -> case toTarget y of
    EventTarget eventId ->
      eventId `member` (game ^. locationsL . ix locId . to (`getSet` game))
    _ -> False
  _ -> False

instance ToEncounterCard Enemy where
  asEncounterCard enemy =
    lookupEncounterCard (getCardCode enemy) (CardId . unEnemyId $ toId enemy)

isInEncounterDiscard
  :: (ToEncounterCard entity) => Game queue -> entity -> Bool
isInEncounterDiscard game entity = card `elem` discard'
 where
  discard' = game ^. discardL
  card = asEncounterCard entity

updatedResourceCount :: Game queue -> Investigator -> Int
updatedResourceCount game investigator =
  game ^?! investigatorsL . ix (toId investigator) . to
    (Investigator.investigatorResources . toAttrs)

evadedBy :: Game queue -> Investigator -> Enemy -> Bool
evadedBy game _investigator enemy =
  let enemy' = game ^?! enemiesL . ix (toId enemy)
  in not (isEngaged enemy') && isExhausted enemy'

hasRemainingActions :: Game queue -> Int -> Investigator -> Bool
hasRemainingActions game n investigator =
  let investigator' = game ^?! investigatorsL . ix (toId investigator)
  in actionsRemaining investigator' == n

hasDamage :: (HasDamage a) => (Int, Int) -> a -> Bool
hasDamage n a = getDamage a == n

hasTrauma :: (HasTrauma a) => (Int, Int) -> a -> Bool
hasTrauma n a = getTrauma a == n

hasDoom :: (TargetEntity a) => Game queue -> Int -> a -> Bool
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

hasEnemy :: Game queue -> Enemy -> Location -> Bool
hasEnemy g e l = toId e `member` getSet @EnemyId l g

hasCardInPlay :: Game queue -> Card -> Investigator -> Bool
hasCardInPlay g c i = case c of
  PlayerCard pc -> case pcCardType pc of
    AssetType -> AssetId (unCardId $ pcId pc) `member` getSet i g
    _ -> error "not implemented"
  _ -> error "not implemented"

hasTreacheryWithMatchingCardCode
  :: (HasSet TreacheryId env a, env ~ Game queue)
  => Game queue
  -> Card
  -> a
  -> Bool
hasTreacheryWithMatchingCardCode g c a = maybe
  False
  (\treachery -> toId treachery `member` getSet a g)
  mtreachery
 where
  mtreachery =
    find ((== getCardCode c) . getCardCode) $ toList (g ^. treacheriesL)

hasClueCount :: HasCount ClueCount () a => Int -> a -> Bool
hasClueCount n a = n == unClueCount (getCount a ())

hasUses :: (HasCount UsesCount () a) => Int -> a -> Bool
hasUses n a = n == unUsesCount (getCount a ())
