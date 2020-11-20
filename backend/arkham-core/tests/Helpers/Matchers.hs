module Helpers.Matchers where

import ClassyPrelude

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
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery
import Arkham.Types.TreacheryId
import qualified Data.List as L
import Lens.Micro

isInDiscardOf
  :: (ToPlayerCard entity) => Game queue -> Investigator -> entity -> Bool
isInDiscardOf game investigator entity = card `elem` discard'
 where
  discard' =
    game ^?! investigators . ix (getInvestigatorId investigator) . to discardOf
  card = asPlayerCard game entity

class ToPlayerCard a where
  asPlayerCard :: Game queue -> a -> PlayerCard

class ToEncounterCard a where
  asEncounterCard :: Game queue -> a -> EncounterCard

instance ToPlayerCard PlayerCard where
  asPlayerCard _ = id

instance ToPlayerCard Event where
  asPlayerCard game event =
    lookupPlayerCard (getCardCode event) (CardId . unEventId $ getId event game)

instance ToPlayerCard Treachery where
  asPlayerCard game treachery = lookupPlayerCard
    (getCardCode treachery)
    (CardId . unTreacheryId $ getId treachery game)

class Entity a => TestEntity a where
  updated :: Game queue -> a -> a

instance TestEntity Agenda where
  updated g a = g ^?! agendas . ix (getAgendaId a)

instance TestEntity Treachery where
  updated g t = g ^?! treacheries . ix (getTreacheryId t)

instance TestEntity Asset where
  updated g a = g ^?! assets . ix (getAssetId a)

instance TestEntity Location where
  updated g l = g ^?! locations . ix (getLocationId l)

instance TestEntity Event where
  updated g e = g ^?! events . ix (getEventId e)

instance TestEntity Enemy where
  updated g e = g ^?! enemies . ix (getEnemyId e)

instance TestEntity Investigator where
  updated g i = g ^?! investigators . ix (getInvestigatorId i)

isAttachedTo :: (TestEntity a, TestEntity b) => Game queue -> a -> b -> Bool
isAttachedTo game x y = case toTarget x of
  LocationTarget locId -> case toTarget y of
    EventTarget eventId ->
      eventId `member` (game ^. locations . ix locId . to (`getSet` game))
    _ -> False
  _ -> False

instance ToEncounterCard Enemy where
  asEncounterCard game enemy = lookupEncounterCard
    (getCardCode enemy)
    (CardId . unEnemyId $ getId enemy game)

isInEncounterDiscard :: (ToEncounterCard entity) => Game queue -> entity -> Bool
isInEncounterDiscard game entity = card `elem` discard'
 where
  discard' = game ^. discard
  card = asEncounterCard game entity

updatedResourceCount :: Game queue -> Investigator -> Int
updatedResourceCount game investigator =
  game ^?! investigators . ix (getInvestigatorId investigator) . to
    (Investigator.investigatorResources . investigatorAttrs)

evadedBy :: Game queue -> Investigator -> Enemy -> Bool
evadedBy game _investigator enemy =
  let enemy' = game ^?! enemies . ix (getEnemyId enemy)
  in not (isEngaged enemy') && isExhausted enemy'

hasRemainingActions :: Game queue -> Int -> Investigator -> Bool
hasRemainingActions game n investigator =
  let
    investigator' =
      game ^?! investigators . ix (getInvestigatorId investigator)
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

hasEnemy :: Game queue -> Enemy -> Location -> Bool
hasEnemy g e l = getId e g `member` getSet @EnemyId l g

hasCardInPlay :: Game queue -> Card -> Investigator -> Bool
hasCardInPlay g c i = case c of
  PlayerCard pc -> case pcCardType pc of
    AssetType -> AssetId (unCardId $ pcId pc) `member` getSet i g
    _ -> error "not implemented"
  _ -> error "not implemented"

hasPassedSkillTestBy :: Int -> GameExternal -> Target -> Investigator -> Bool
hasPassedSkillTestBy n game target investigator = hasProcessedMessage
  (PassedSkillTest
    (getInvestigatorId investigator)
    Nothing
    TestSource
    (SkillTestInitiatorTarget target)
    n
  )
  game

hasTreacheryWithMatchingCardCode
  :: (HasSet TreacheryId env a, env ~ Game queue)
  => Game queue
  -> Card
  -> a
  -> Bool
hasTreacheryWithMatchingCardCode g c a = maybe
  False
  (\treachery -> getId @TreacheryId treachery g `member` getSet a g)
  mtreachery
 where
  mtreachery =
    find ((== getCardCode c) . getCardCode) $ toList (g ^. treacheries)

hasClueCount :: HasCount ClueCount () a => Int -> a -> Bool
hasClueCount n a = n == unClueCount (getCount a ())

hasUses :: (HasCount UsesCount () a) => Int -> a -> Bool
hasUses n a = n == unUsesCount (getCount a ())
