{- | Building log refs that need to look something up.

Note the namespace: "Arkham.Helpers.Log" is already taken, and is about the
/campaign/ log (@getCampaignLog@, @hasCampaignOption@). Nothing here relates to
it.

"Arkham.Log" is pure: it takes a name and an id and makes a chip. The narrator
has neither — it reads messages, which carry ids. These helpers close that gap.

Everything here uses 'fieldMay', never 'field'. The narrator runs inside
@runMessages@ and must not throw, and an id in a message is routinely for an
entity that has already left play by the time the log is written: an enemy that
was just defeated, a location that was just replaced. A ref that falls back to
the bare id is a worse chip; a crash is a broken game.
-}
module Arkham.Log.Refs where

import Arkham.Act.Types (Field (..))
import Arkham.Agenda.Types (Field (..))
import Arkham.Asset.Types (Field (..))
import Arkham.Card

-- EntityId is an associated type of Entity, so it imports as a subordinate
-- name rather than on its own.
import Arkham.Classes.Entity (Entity (EntityId))
import Arkham.Classes.GameLogger (HasGameLogger)
import Arkham.Classes.HasGame
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Field (..))

{- The 'Projection' instances live in "Arkham.Game", which imports the narrator,
which imports this module. The boot file declares them, so taking them from
there breaks the cycle -- the same trick "Arkham.Investigate" uses. -}
import {-# SOURCE #-} Arkham.Game ()
import Arkham.GameEnv (getCardMaybe, getCardPlayStack, getSkillTest)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Log
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillTest.Base (SkillTest)
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Types (Field (..))

{- | A location chip, drawing its back when the location is not yet revealed --
which is why this needs game state and the pure constructor cannot do it.
-}
locationRefFor :: HasGame m => LocationId -> m LogRef
locationRefFor lid = do
  mCard <- fieldMay LocationCard lid
  revealed <- fromMaybe False <$> fieldMay LocationRevealed lid
  pure $ case mCard of
    Just card -> locationRef lid (toName card) (toCardCode card) revealed
    Nothing -> fallbackRef RefLocation (idText lid)

{- | An investigator chip with its real name.

Reads the name rather than leaving the card code as a fallback, because the
client cannot always resolve it: a __custom investigator__ has a @*@-prefixed
code that is in nobody's card database, so the log showed
@*7a1f2c9e4b6d48f0a3c5e7d9b1f3a5c70 fails by 1 fighting Icy Ghoul@. The engine
always knows the name; the client only sometimes does.
-}
investigatorRefFor :: HasGame m => InvestigatorId -> m LogRef
investigatorRefFor iid = do
  mName <- fieldMay InvestigatorName iid
  pure $ maybe (investigatorRefById iid) (investigatorRef iid) mName

enemyRefFor :: HasGame m => EnemyId -> m LogRef
enemyRefFor eid = fromCard RefEnemy EnemyCard eid (enemyRef eid)

assetRefFor :: HasGame m => AssetId -> m LogRef
assetRefFor aid = fromCard RefAsset AssetCard aid (assetRef aid)

treacheryRefFor :: HasGame m => TreacheryId -> m LogRef
treacheryRefFor tid = fromCard RefTreachery TreacheryCard tid (treacheryRef tid)

eventRefFor :: HasGame m => EventId -> m LogRef
eventRefFor eid = fromCard RefEvent EventCard eid (eventRef eid)

actRefFor :: HasGame m => ActId -> m LogRef
actRefFor aid = do
  mCard <- fieldMay ActCard aid
  pure $ case mCard of
    Just card -> actRef aid (toName card) (toCardCode card)
    Nothing -> byCardCodeRef RefAct (toCardCode aid)

agendaRefFor :: HasGame m => AgendaId -> m LogRef
agendaRefFor aid = do
  mCard <- fieldMay AgendaCard aid
  pure $ case mCard of
    Just card -> agendaRef aid (toName card) (toCardCode card)
    Nothing -> byCardCodeRef RefAgenda (toCardCode aid)

{- | A ref for an act or agenda that is no longer in play.

Their ids ARE card codes, and an act is replaced on the spot when it advances,
so an effect of the act that just advanced has no entity left to read a name
off -- which is how the log came to say "takes 1 damage from c03047a". The
printed definition is still there to ask.

The client cannot rescue this one: its card index carries player and encounter
cards, not acts and agendas, so the name has to come from here.
-}
byCardCodeRef :: LogRefKind -> CardCode -> LogRef
byCardCodeRef kind cc = case lookupCardDef cc of
  Just def -> (logRef kind (display $ toName def)) {logRefCardCode = Just cc}
  Nothing -> fallbackRef kind (unCardCode cc)

{- | Build a ref from the entity's @Card@.

Every in-play entity has one, and it carries both the name and the card code --
unlike the per-entity @*Name@ / @*CardCode@ fields, most of which do not exist.
One shape for all of them, and a new entity kind needs no new lookup logic.
-}
fromCard
  :: forall a m
   . (HasGame m, Projection a, ToJSON (EntityId a))
  => LogRefKind
  -> Field a Card
  -> EntityId a
  -> (Name -> CardCode -> LogRef)
  -> m LogRef
fromCard kind fld eid build = do
  mCard <- fieldMay fld eid
  pure $ case mCard of
    Just card -> build (toName card) (toCardCode card)
    Nothing -> fallbackRef kind (idText eid)

{- | A chip for whatever a 'Target' points at, when it is something a reader
would recognise. 'Nothing' for the targets that are plumbing -- a batch, a
window, the game itself -- so the caller can leave them out of the sentence
rather than print a uuid.
-}
targetRefFor :: HasGame m => Target -> m (Maybe LogRef)
targetRefFor = \case
  LocationTarget lid -> Just <$> locationRefFor lid
  EnemyTarget eid -> Just <$> enemyRefFor eid
  AssetTarget aid -> Just <$> assetRefFor aid
  TreacheryTarget tid -> Just <$> treacheryRefFor tid
  EventTarget eid -> Just <$> eventRefFor eid
  ActTarget aid -> Just <$> actRefFor aid
  AgendaTarget aid -> Just <$> agendaRefFor aid
  InvestigatorTarget iid -> Just <$> investigatorRefFor iid
  -- The test's own target is a wrapper around the real one.
  SkillTestInitiatorTarget t -> targetRefFor t
  ProxyTarget t _ -> targetRefFor t
  _ -> pure Nothing

-- | The same, for a 'Source'. Peels the wrappers that carry one.
sourceRefFor :: HasGame m => Source -> m (Maybe LogRef)
sourceRefFor = \case
  LocationSource lid -> Just <$> locationRefFor lid
  EnemySource eid -> Just <$> enemyRefFor eid
  EnemyAttackSource eid -> Just <$> enemyRefFor eid
  AssetSource aid -> Just <$> assetRefFor aid
  TreacherySource tid -> Just <$> treacheryRefFor tid
  EventSource eid -> Just <$> eventRefFor eid
  ActSource aid -> Just <$> actRefFor aid
  AgendaSource aid -> Just <$> agendaRefFor aid
  InvestigatorSource iid -> Just <$> investigatorRefFor iid
  -- A card with no entity behind it: the thing that surged, most often.
  CardIdSource cid -> fmap toLogRef <$> getCardMaybe cid
  AbilitySource s _ -> sourceRefFor s
  UseAbilitySource _ s _ -> sourceRefFor s
  ProxySource s _ -> sourceRefFor s
  IndexedSource _ s -> sourceRefFor s
  PaymentSource s -> sourceRefFor s
  BothSource s _ -> sourceRefFor s
  _ -> pure Nothing

{- | Send an entry, filed under whatever block is open.

Anything that happens /during/ a test or a card play -- the clue it discovered,
the tokens it revealed, what it cost -- belongs inside that block rather than
beside it, and the sender should not have to know one is running. With nothing
open this is exactly 'sendLog'.

__Membership is what keeps a block in one piece.__ The client only merges a
/contiguous/ run of rows sharing a group id (@groupLogEntries@), so an entry
that happens mid-block and does not join it does not merely sit outside: it
splits the block in two.
-}
sendLogInOpenBlock :: (HasGame m, HasGameLogger m) => LogEntry -> m ()
sendLogInOpenBlock entry
  -- An entry that already says which group it belongs to keeps its role: a
  -- block's own header and summary set theirs explicitly.
  | isJust entry.logEntryGroup = sendLog entry
  | otherwise = do
      mkey <- openBlockKey
      sendLog $ maybe entry (`inGroupOf` entry) mkey

{- | The block an entry should join, if any.

A skill test wins over a card play: playing a card to commit to a test happens
inside the test, not the other way round.
-}
openBlockKey :: HasGame m => m (Maybe Text)
openBlockKey =
  getSkillTest >>= \case
    Just st -> pure $ Just (skillTestLogKey st)
    Nothing -> fmap cardBlockKey . lastMay <$> getCardPlayStack

-- | The key a skill test's block is filed under. Shared with the narrator.
skillTestLogKey :: SkillTest -> Text
skillTestLogKey st = "skillTest:" <> tshow st.id

{- | The key the block around a card is filed under: a card being played, or an
encounter card being drawn and resolved. Shared with the narrator.
-}
cardBlockKey :: CardId -> Text
cardBlockKey cid = "cardPlay:" <> tshow cid

{- | Last resort when the entity is gone: the id, so the line still names
something stable, and the client can still try its own lookup.
-}
fallbackRef :: LogRefKind -> Text -> LogRef
fallbackRef kind = logRef kind
