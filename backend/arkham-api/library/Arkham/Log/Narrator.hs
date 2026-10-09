{- | Deriving the game log from the message stream.

The log cannot be written per card: measured, 39 log call sites against 5,634
card implementations, 0.7%. It has to be /derived/ from the messages the engine
already processes. See @docs/game-log/@.

== Why a state machine

A player-visible event is almost never one message. A skill test spans a
@BeginSkillTest@, any number of commits and token reveals, and a result -- and
measured over 3,111 real messages, about 64% are pure plumbing (@Do@,
@CheckWindows@, @ClearUI@, @Ask@) that must produce nothing at all. A flat
@case@ over one message can say "a test happened"; it can never say "Daisy
fails by 2".

So each multi-message event is a small machine. It opens on a message it
recognises, accumulates while it waits, and emits exactly once when it has
enough. Anything it does not recognise it ignores. If the state stops making
sense it 'Bail's and says nothing rather than guessing.

== Frames last one action, and most events do not

Measured, not assumed: a skill test spans __four__ HTTP actions (open the test,
commit cards, draw the token, apply results), because each ask ends an action.
The narrator ref is created per action in @updateGame@, so a frame opened while
the test begins is __gone__ by the time the result arrives. The instrumented
run was unambiguous: one @OPEN@, five @Wait@, nineteen @Ignore@, zero @EMIT@ --
and then the result messages turned up with the stack empty.

So frames are only for events that resolve __inside one action__. Anything that
spans an ask must emit from a __single__ message, reading the engine's own
state if it needs more; the engine already keeps a @SkillTest@ across the whole
test, so duplicating that here would be both redundant and wrong.

That is what 'oneShot' is for, and why the skill test lives there.

== Three rules this file must keep

1. __Silence is the default.__ A message no machine claims produces nothing.
   There is deliberately no catch-all rendering: a fallback that narrated
   unknown messages would bury the log in the 64% that is plumbing.

2. __Emit once.__ Several of these messages fan out to every participant. A
   single skill test sends @PassedSkillTest_@ to the committed skill, the
   investigator and each revealed chaos token. Only the one addressed to
   'SkillTestInitiatorTarget' is the result, and a frame /closes/ when it
   emits, so a repeat finds no open frame and is dropped.

3. __Never break the game.__ This runs inside @runMessages@. It reads state and
   builds text; it must not throw, must not push messages, and must not depend
   on an entity still existing. Everything it needs comes off the messages
   themselves.
-}
module Arkham.Log.Narrator (
  Narrator,
  emptyNarrator,
  narrationFor,
  placeNarration,
  flushNarrator,
) where

import Arkham.Ability.Type (AbilityType (..))
import Arkham.Ability.Types qualified as Ab
import Arkham.Act.Sequence qualified as A
import Arkham.Act.Types (Field (..))
import Arkham.Action
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Field (..))
import Arkham.Attack.Types
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Classes.Query (select)
import Arkham.Constants (notPlayerAbilityIndex)
import Arkham.Cost (Cost (ActionCost, ResourceCost))
import Arkham.Enemy.Types (Field (..))
import Arkham.Game.Base (Game (..))
import Arkham.Game.Utils (modeScenario)
import Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Scenario (getInResolution)
import Arkham.Helpers.SkillTest (
  calculateSkillTestResultsData,
  getModifiedSkillTestDifficulty,
 )
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword (Keyword (Peril))
import Arkham.Location.Types (Field (..))
import Arkham.Log
import Arkham.Log.Refs
import Arkham.Matcher (InvestigatorMatcher (ActiveInvestigator), LocationMatcher, replaceYouMatcher)
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Movement (moveForced)
import Arkham.Phase
import Arkham.Prelude
import Arkham.Projection
import Arkham.Resolution

-- For the @HasField "id"@/@"name"@ instances on 'Arkham.Scenario.Types.Scenario'.
import Arkham.Scenario.Types ()
import Arkham.SkillTest.Base (SkillTest (..), SkillTestResultsData (..))
import Arkham.SkillTest.Type
import Arkham.Source
import Arkham.Spawn
import Arkham.Target
import Data.Text qualified as T

-- * State

{- | What the narrator is carrying between messages.

__Costs, and nothing else.__ The original design was a stack of frames -- open
on a message, accumulate, emit when you have enough -- and those were deleted
because, as the note above records, a frame cannot survive the ask that ends
the action it was opened in.

A cost is the one thing that genuinely has to be carried: it has to be
announced where it is computed (see @AdditionalCostPaid@), which is /before/
the thing it paid for happens, so sending it straight out would print "+1
action from Frozen in Fear" above a move the log has not described yet. Held
instead, and attached to the next thing narrated, so the two read as one entry.

__This hold only works when no ask intervenes, and that is not something to
assume.__ Research Librarian disproved the comfortable version of this claim: a
@freeReaction@ on @AssetEntersPlay #after@ ends the action between paying for
the card and resolving it, so the hold flushed and the payment stranded itself
above a play the log had not written. Anything that must survive an ask belongs
in game state, where @gameCardPlayStack@ puts it -- carrying context that is
gone by the time the entry can be built is exactly what the state-machine
design was for, and the per-action ref is the wrong place to keep it.

'flushNarrator' empties this at the end of every action, so a cost whose action
produced no entry at all is still said, standalone, rather than dropped with
the ref.
-}
newtype Narrator = Narrator {narratorPendingCosts :: [LogEntry]}

emptyNarrator :: Narrator
emptyNarrator = Narrator []

-- * Driver

{- | Offer one message to the narrator.

Called for every message popped, before it runs. Reads nothing from game state,
so it cannot fail on a missing entity, and sends at most one entry.
-}

{- | Send a built narration, or hold it back.

__This, not 'sendLogInOpenBlock', is what the engine calls.__ Every narration has
to come through here, because this is the only thing that knows about held
costs; a second path straight to the logger silently defeats them.
-}
placeNarration
  :: (HasGame m, HasGameLogger m) => IORef Narrator -> Message -> LogEntry -> m ()
placeNarration ref msg entry
  | isCostNarration msg = holdCost ref entry
  | otherwise = emitWithHeldCosts ref entry

{- | Messages whose narration is a price rather than an event.

Every one of these is announced before the thing it bought -- resources leave
your pool on the way to playing the card, not after -- so sending them straight
out puts the cost above an entry the log has not written yet. Held instead, and
attached to whatever the payment turns out to have been for.
-}
isCostNarration :: Message -> Bool
isCostNarration = \case
  AdditionalCostPaid {} -> True
  SpendResources {} -> True
  _ -> False

{- | A surcharge waits for the thing it paid for -- unless a block is already
open, in which case it belongs in that block, now, next to the rest of the
action it is part of.
-}
holdCost :: (HasGame m, HasGameLogger m) => IORef Narrator -> LogEntry -> m ()
holdCost ref entry = do
  mkey <- openBlockKey
  case mkey of
    Just _ -> sendLogInOpenBlock entry
    Nothing -> modifyIORef' ref \n -> n {narratorPendingCosts = n.narratorPendingCosts <> [entry]}

-- | Send an entry, carrying any held costs with it.
emitWithHeldCosts :: (HasGame m, HasGameLogger m) => IORef Narrator -> LogEntry -> m ()
emitWithHeldCosts ref entry = do
  held <- takeHeldCosts ref
  case held of
    [] -> sendLogInOpenBlock entry
    _ | not (canOwnCosts entry) -> do
      -- Nothing here paid for anything, so the costs stand on their own rather
      -- than being adopted by whatever happened to come next.
      traverse_ sendLog held
      sendLogInOpenBlock entry
    _ -> case entry.logEntryGroup of
      {- A block draws its header's body and nothing else, so a cost attached as a
      child of one would never be seen. It goes in as the block's first member
      instead, which is where a reader looking for why the action cost what it
      did would expect it. -}
      Just g | g.logGroupRole == GroupHeader -> do
        sendLogInOpenBlock entry
        traverse_ (sendLog . inGroupOf g.logGroupId) held
      _ -> sendLogInOpenBlock $ withChildren held entry

{- | Whether an entry is the kind of thing a payment can belong to.

Something a player chose to do, or a block being opened by one. A payment is
otherwise left alone: a phase banner, a damage line or a campaign-log write
would adopt it just as happily, and "Mythos phase / spends 2 resources" is
worse than the two lines apart.
-}
canOwnCosts :: LogEntry -> Bool
canOwnCosts entry =
  entry.logEntryKind == Action || any ((== GroupHeader) . logGroupRole) entry.logEntryGroup

{- | Say anything still held at the end of an action.

Nothing claimed it -- a cost paid for something the log has no narration for --
so it goes out on its own rather than being dropped or leaking into whatever
the next action happens to narrate first.
-}
flushNarrator :: HasGameLogger m => IORef Narrator -> m ()
flushNarrator ref = takeHeldCosts ref >>= traverse_ sendLog

takeHeldCosts :: MonadIO m => IORef Narrator -> m [LogEntry]
takeHeldCosts ref =
  atomicModifyIORef' ref \n -> (n {narratorPendingCosts = []}, n.narratorPendingCosts)

{- | The narration a message calls for, if any.

Split from 'placeNarration' so the /match/ stays pure: the @Maybe@ is decided without
touching game state, and only a message that actually narrates pays for a
'Arkham.GameEnv.runWithEnv'. That matters -- this runs for every message, and
about 64% of them are plumbing.
-}
narrationFor :: (HasGame m, HasGameLogger m) => Message -> Maybe (m (Maybe LogEntry))
narrationFor = oneShot

-- * One-shot narrations

{- | Events narrated from a single message, with no frame.

This is where anything that spans an ask belongs, which in practice is most of
what a player sees.
-}

{- | 'HasGameLogger' as well as 'HasGame', because a narration may also take a
line back: an uncommit retracts the commit's entry and returns 'Nothing'. That
is still not pushing a game message -- rule 3 of the module header stands.
-}
oneShot :: (HasGame m, HasGameLogger m) => Message -> Maybe (m (Maybe LogEntry))
oneShot = \case
  -- The skill test's result.
  --
  -- Two conditions, and the entry is wrong without both. Measured on one real
  -- test, the identical payload arrives FIVE times:
  --
  -- >  Will  (SkillTestMessage (PassedSkillTest_ ... SkillTestInitiatorTarget ...))
  -- >  When  (...)
  -- >  After (...)
  -- >         SkillTestMessage (PassedSkillTest_ ... SkillTestInitiatorTarget ...)   <- this one
  -- >  Do (After (...))
  --
  -- 1. __Unwrapped.__ @Will@, @When@, @After@ and @Do@ surround the real
  --    occurrence; matching them narrates one test four extra times. Only the
  --    bare message is the event itself.
  -- 2. __'SkillTestInitiatorTarget'.__ The engine also fans the result out to
  --    every participant -- each committed skill, the investigator, every
  --    revealed chaos token -- so they can react. 36 copies for three tests in
  --    one trace. Only the copy addressed to the initiator is the test's own.
  --
  -- Together they fire exactly once per test, which is why this needs no state.
  {- A test opening.

  @BeginSkillTestWithPreMessages'_@ is where the test is actually installed
  (@Game/Runner.hs:3193@); the other @BeginSkillTest*@ shapes delegate to it, so
  this fires exactly once per test -- including a repeat, which is a new test
  and deserves a new block.

  The narrator runs BEFORE the message executes, so there is no active skill
  test to read yet. It does not matter: the message carries the whole record. -}
  SkillTestMessage (BeginSkillTestWithPreMessages'_ _ st) -> Just (renderSkillTestOpening st)
  SkillTestMessage (BeginSkillTestWithPreMessages_ _ _ st) -> Just (renderSkillTestOpening st)
  -- NOT @StartSkillTest_@ as well. It was matched too while the block was a
  -- single row revised in place, where a second opening was harmless; now that
  -- every send is its own row it would give the block two headers.
  --
  -- Matching both @BeginSkillTest*@ shapes is still exactly one header: the
  -- un-primed one is turned into the primed one by a direct @runMessage@
  -- (@Game/Runner.hs:3192@), so only ever one of the two reaches the queue.
  SkillTestMessage (PassedSkillTest_ iid mAction _ (SkillTestInitiatorTarget t) sType n) ->
    Just (renderSkillTestResult iid mAction t sType True n)
  SkillTestMessage (FailedSkillTest_ iid mAction _ (SkillTestInitiatorTarget t) sType n) ->
    Just (renderSkillTestResult iid mAction t sType False n)
  -- A card committed, as a line inside the open test's block.
  SkillTestMessage (SkillTestCommitCard_ iid card) -> Just do
    who <- investigatorRefFor iid
    mKey <- openSkillTestKey
    pure $ flip fmap mKey \k ->
      inGroupOf k
        -- Tagged by the card, so uncommitting takes the line back out.
        $ tagged (commitLogTag card)
        $ notice [ikeyPart "log.commits" ["investigator" ~> who, "card" ~> card]]
  {- A card taken back off a test.

  The commit never happened, so its line should not survive it. This is the one
  narration that /removes/ rather than adds; see 'retractLog'. -}
  SkillTestMessage (SkillTestUncommitCard_ _ card) -> Just do
    retractLog (commitLogTag card)
    pure Nothing
  {- Cards committed to a test.

  @SkillTestCommitCard_@, not @CommitCard_@: the latter is the request, and the
  former is what @SkillTest/Runner.hs:657@ files under
  @skillTestCommittedCards@ once the commit is allowed.

  Deliberately NOT narrated on its own -- it is read back off the test and hung
  under the result (see 'renderSkillTestResult'), which is the whole point of
  nesting. Left here so the next reader does not "fix" the omission. -}
  {- Damage and horror, in one line.

  @AssignedDamage@, not @Damaged_@ and not @PlaceTokens_@. Both of those were
  tried and neither fires for an investigator: a full enemy attack with damage
  assigned produced 12 @PlaceTokens@ messages, every one of them Doom or Clue.
  @Investigator/Runner/Damage.hs:1093@ is where it actually lands, and it
  pushes this.

  It carries damage and horror together, which is also how a player experiences
  an attack -- one event, not two, and it names what did it. -}
  AssignedDamage target source damage horror
    | damage > 0 || horror > 0 -> Just (renderAssignedDamage target source damage horror)
  {- Where an enemy spawns being put to a player.

  __Two messages ask this, and the obvious one is the rare one.__ A card's own
  @setSpawnAt@ matcher never reaches @Do (EnemySpawn ...)@ as a matcher at all:
  @Helpers/Enemy.hs:75@ routes @SpawnAt@ through
  @EnemySpawnAtLocationMatching@, which selects and hands off to
  @spawnAtOneOf@ (@Helpers/Enemy.hs:407@) -- read off a live game, where
  Dancing Rats' pending ask carried @SpawnAtLocation@ choices and so had
  already left the matcher behind. @Do (EnemySpawn ...)@ keeps a matcher only
  for a spawn a modifier redirected (@ChangeSpawnWith@ / @ChangeSpawnLocation@,
  @Enemy/Runner.hs:479@), which asks at @Enemy/Runner.hs:511@. Both are live;
  neither covers the other.

  Either way the prompt is only worth a line when more than one location
  matches -- a single match resolves without asking, so a line there would
  describe a choice nobody made. The matcher is re-selected rather than
  carried, which is safe because the narrator runs /before/ the message does
  and so reads the same board the ask will. -}
  SpawnMessage (EnemySpawnAtLocationMatching_ _ matcher eid) -> Just do
    -- The handler resolves @You@ against the active investigator before
    -- selecting, so the count has to be taken off the same matcher. Via
    -- 'select' rather than @getActiveInvestigatorId@, whose 'selectJust' throws.
    select ActiveInvestigator >>= \case
      [iid] -> renderSpawnChoice eid (replaceYouMatcher iid matcher)
      _ -> pure Nothing
  Do (EnemySpawn details)
    | SpawnAt matcher <- details.spawnAt -> Just (renderSpawnChoice details.enemy matcher)
  -- An enemy arriving. @EnemySpawn_@ is the request; @EnemySpawned_@ is the fact,
  -- and by then the enemy has a location to read.
  SpawnMessage (EnemySpawned_ details) -> Just (renderSpawned details)
  -- Something leaving play for good.
  DefeatMessage (Defeated_ target _ source _) -> Just (renderDefeated target source)
  InvestigatorMessage (InvestigatorDefeated_ source iid) ->
    Just (renderDefeated (InvestigatorTarget iid) source)
  -- \* Structure: the headings a reader orients by.
  {- The scenario's own title banner.

  @LoadScenario@, not @StartScenario@: the latter is what /builds/ the scenario,
  so when the narrator sees it there is no entity to read a name off yet. By
  @LoadScenario@ there is, and it still lands before the intro flavour and the
  setup. -}
  LoadScenario {} -> Just do
    mScenario <- modeScenario . gameMode <$> getGame
    pure $ flip fmap mScenario \s ->
      structure [ikeyPart "log.scenarioBegins" ["scenario" ~> scenarioRef s.id s.name]]
  Begin phase -> Just (pure $ Just $ structure [ikeyPart (phaseKey phase) []])
  {- The round number.

  Read AFTER the increment would be wrong -- the narrator runs before the
  message does, so the counter is still the previous round's. Add one. -}
  {- Whose turn it is.

  No number: the engine counts neither rounds nor turns, and for a turn the
  investigator's name is the useful heading anyway. -}
  BeginTurn iid -> Just do
    who <- investigatorRefFor iid
    pure $ Just $ structure [ikeyPart "log.turn" ["investigator" ~> who]]
  BeginRound -> Just do
    n <- gameRoundCount <$> getGame
    pure $ Just $ structure [ikeyPart "log.round" ["count" ~> (n + 1)]]
  -- \* Doing things
  {- Playing a card, as a block: the play, what it cost, and anything the card
  did on the way in.

  The header arrives LAST -- the cost is paid before the card resolves -- and
  that is fine, because the client assigns header/member/summary by role rather
  than by arrival order. What it does need is for every row in between to carry
  the same group id, which is what @gameCardPlayStack@ and
  'Arkham.Log.Refs.openBlockKey' are for; a row that does not join splits the
  block in two. -}
  ResolvedPlayCard iid card -> Just do
    who <- investigatorRefFor iid
    pure
      $ Just
      $ opensGroup (cardBlockKey card.id)
      $ action [ikeyPart "log.playsCard" ["investigator" ~> who, "card" ~> card]]
  {- Movement.

  @EnterLocation@ after all. It __is__ pushed -- from @handleDoResolveMovement@
  in @Investigator/Runner/Movement.hs@ -- but inside
  @Simultaneously $ Run [...]@, and @Simultaneously@ ran its sub-messages past
  the narrator (fixed in @Arkham.Game@). @MoveTo@ is the intent and can still be
  refused; @PlaceInvestigator@ only fires for vehicles and a few specific
  cards. -}
  EnterLocation iid lid mMovement -> Just do
    who <- investigatorRefFor iid
    loc <- locationRefFor lid
    {- Forced moves read differently. "Daisy Walker moves to Attic" is something
    she decided; a card dragging her there is not, and the log saying she did it
    invites the reader to look for the action she spent. -}
    let key = if maybe False moveForced mMovement then "log.isMovedTo" else "log.movesTo"
    pure $ Just $ action [ikeyPart key ["investigator" ~> who, "location" ~> loc]]
  {- An extra cost a card imposed.

  Held by 'placeNarration' rather than sent, so it lands under the action it paid for
  instead of above it. The investigator is not named because by the time this is
  read it sits beneath an entry that already named them. -}
  AdditionalCostPaid _ src cost -> Just do
    mRef <- sourceRefFor src
    pure do
      ref <- mRef
      (key, n) <- surchargeWording cost
      pure $ notice [ikeyPart key ["count" ~> n, "source" ~> ref]]
  {- Resources gained.

  Only the @False@ variant. @True@ is the resource /action/, and its handler
  pushes a @False@ one to do the actual gaining
  (@Investigator/Runner/Action.hs:289@) -- so matching both logged every resource
  action twice. -}
  TakeResources iid n _ False | n > 0 -> Just do
    who <- investigatorRefFor iid
    pure $ Just $ mechanic [ikeyPart "log.gainsResources" ["investigator" ~> who, "count" ~> n]]
  EvadeMessage (EvadedTarget_ iid target) -> Just do
    who <- investigatorRefFor iid
    mRef <- targetRefFor target
    pure $ flip fmap mRef \t ->
      mechanic [ikeyPart "log.evades" ["investigator" ~> who, "target" ~> t]]

  -- \* Enemies acting
  EngageMessage (EnemyEngageInvestigator_ eid iid) -> Just do
    enemy <- enemyRefFor eid
    who <- investigatorRefFor iid
    pure $ Just $ mechanic [ikeyPart "log.engages" ["enemy" ~> enemy, "investigator" ~> who]]
  HuntMessage (EnemyMove_ eid lid) -> Just do
    enemy <- enemyRefFor eid
    loc <- locationRefFor lid
    pure $ Just $ mechanic [ikeyPart "log.enemyMovesTo" ["enemy" ~> enemy, "location" ~> loc]]
  EnemyAttackMessage (EnemyAttack_ details) -> Just (renderEnemyAttack details)
  -- \* The encounter deck
  {- An encounter card drawn.

  NOT treacheries: the dispatcher that handles this pushes @DrewTreachery@ for
  one (@Game/Runner.hs:3816@), and that is narrated below with the Peril rule.
  Matching both would log every treachery draw twice.

  Both variants, because the deck-aware one is what the encounter phase actually
  pushes (@Scenario/Runner.hs:988@) -- matching only the plain one dropped every
  enemy and asset drawn from the encounter deck out of the log. No double entry:
  the plain variant reaches the other through a direct @runMessage@
  (@Game/Runner.hs:3747@), which never passes the queue the narrator reads. -}
  InvestigatorMessage (InvestigatorDrewEncounterCard_ iid card)
    | cdCardType (toCardDef card) /= TreacheryType -> Just (drawsEncounter iid card)
  InvestigatorMessage (InvestigatorDrewEncounterCardFrom_ iid card _)
    | cdCardType (toCardDef card) /= TreacheryType -> Just (drawsEncounter iid card)
  {- A treachery drawn, and the one place the log deliberately tells two seats
  different things.

  Peril is resolved alone -- nobody else may help -- so naming the card to the
  table would hand them information the rules are withholding. The drawer gets
  the card; everyone else is told that a Peril was drawn, which is what they are
  entitled to know.

  Two entries, so the public one is sent here and the private one returned. -}
  DrewTreachery iid _ card -> Just do
    who <- investigatorRefFor iid
    mPlayer <- fieldMay InvestigatorPlayerId iid
    let
      named =
        opensGroup (cardBlockKey card.id)
          $ mechanic [ikeyPart "log.drawsTreachery" ["investigator" ~> who, "card" ~> card]]
    if Peril `member` cdKeywords (toCardDef card)
      then do
        sendLog
          $ opensGroup (cardBlockKey card.id)
          $ mechanic [ikeyPart "log.drawsPeril" ["investigator" ~> who]]
        pure $ (`forPlayer` named) <$> mPlayer
      else pure $ Just named

  {- Healing.

  @Do (HealDamage ...)@, not the bare constructor: the bare one is the request
  and passes through a @Would@ window where another card can grow or cancel it,
  and @Helpers/Investigator.hs:715@ rewrites the queued message when it does.
  The @Do@ wrapper is what the runner finally handles
  (@Investigator/Runner.hs:1924/1931@), so it is the only form that reports the
  amount actually healed. -}
  Do (HealDamage target _ n) | n > 0 -> Just (renderHealed "log.healsDamage" target n)
  Do (HealHorror target _ n) | n > 0 -> Just (renderHealed "log.healsHorror" target n)
  -- Clues spent, which is nearly always an act being pushed along.
  InvestigatorMessage (InvestigatorSpendClues_ iid n) | n > 0 -> Just do
    who <- investigatorRefFor iid
    pure $ Just $ mechanic [ikeyPart "log.spendsClues" ["investigator" ~> who, "count" ~> n]]
  {- Doom, which is every scenario's clock.

  @PlaceDoom@ and not @PlaceDoomOnAgenda@: the latter resolves by pushing the
  former at whichever agenda is unflipped (@Scenario/Runner.hs:339@), so
  narrating both says the mythos phase's doom twice -- and matching the general
  one also catches doom landing on an enemy or a location, and names it. -}
  PlaceDoom _ target n | n > 0 -> Just (renderTokens "log.placesDoom" target n)
  RemoveDoom _ target n | n > 0 -> Just (renderTokens "log.removesDoom" target n)
  {- A player's own words.

  The one narration that is not derived from anything: the message exists to
  carry it, so it is a straight render. Kept here rather than in a handler so
  the chat line lands in the log in message order with everything else. -}
  ChatMessage iid mSpeaker text -> Just do
    -- The account name when the API resolved one, because a player says this,
    -- not their investigator. Falls back to the investigator rather than going
    -- unattributed.
    speaker <- maybe (toLogPart <$> investigatorRefFor iid) (pure . lit) mSpeaker
    -- Trimmed and capped here rather than trusting the client's maxlength: this
    -- is the copy that becomes a durable row.
    let said = T.take 500 (T.strip text)
    -- The investigator rides along as the entry's source, which is what lets the
    -- client colour the speaker's name by their class. The name itself is the
    -- account's, so it cannot carry that on its own.
    who <- investigatorRefFor iid
    pure
      $ if T.null said
        then Nothing
        else Just $ because who $ chat [speaker, lit ": ", lit said]
  {- A card drawn into hand.

  Addressed to the drawer alone. A hand is hidden information, so this is the
  first narration to use 'LogAudience' -- and the reason the field exists: the
  old log dropped every @ClientCardOnly@ outright, so "you drew Flashlight"
  could never enter history at all. Another seat sees nothing rather than a
  redacted line, which is also what the table sees.

  @InvestigatorDrewPlayerCardFrom_@ is the fact; it carries the card, so no
  lookup can come back empty. -}
  InvestigatorDrewPlayerCardFrom iid card _ msource -> Just do
    who <- investigatorRefFor iid
    mPlayer <- fieldMay InvestigatorPlayerId iid
    -- "from Perception", when the draw had a cause worth naming. A plain upkeep
    -- draw has none, and saying so would be noise.
    mSource <- maybe (pure Nothing) sourceRefFor msource
    let
      (key, extra) = case mSource of
        Just src -> ("log.drawsCardFrom", [("source", toLogPart src)])
        Nothing -> ("log.drawsCard", [])
      entry = mechanic [ikeyPart key (("investigator" ~> who) : ("card" ~> toCard card) : extra)]
    pure $ Just $ maybe entry (`forPlayer` entry) mPlayer
  {- Something discarded.

  @Discarded@, not @DiscardCard@: the latter is the request, and this is what
  the asset, event, location and enemy runners push once the card has actually
  gone. It carries the card, so no lookup can come back empty. -}
  Discarded _ source card -> Just do
    mSource <- sourceRefFor source
    let
      selfInflicted = (logRefCardCode <$> mSource) == Just (Just (toCardCode card))
      (key, extra) = case mSource of
        -- A card that discards itself does not need telling on.
        Just src | not selfInflicted -> ("log.discardedBy", [("source", toLogPart src)])
        _ -> ("log.discarded", [])
    pure $ Just $ mechanic [ikeyPart key (("card" ~> card) : extra)]
  -- Cards leaving a hand, as one line however many went.
  DiscardedCards iid _ _ cards | notNull cards -> Just do
    who <- investigatorRefFor iid
    pure
      $ Just
      $ mechanic
        [ ikeyPart
            "log.discardsCards"
            [ "investigator" ~> who
            , "cards" ~> LogList (map toLogPart cards)
            , "count" ~> length cards
            ]
        ]
  {- Paying for something.

  A 'notice', not a 'mechanic', and held rather than sent: it is the price of
  the thing on the line above it, not an event of its own. See 'isCostNarration'.
  Standalone is still a valid outcome -- a card that just takes resources off
  you pays for nothing -- which is why the investigator stays named. -}
  SpendResources iid n | n > 0 -> Just do
    who <- investigatorRefFor iid
    pure $ Just $ notice [ikeyPart "log.spendsResources" ["investigator" ~> who, "count" ~> n]]
  {- An ability being used.

  @UseAbility@, not @UseCardAbility@: only this one carries the whole 'Ability',
  and the wording has to come off 'abilityType' -- a Forced trigger and a paid
  action read nothing alike. @UseCardAbility@ carries just a source and an
  index, which would need the ability looked back up out of game state.

  The trade is that this fires when the player commits to the ability rather
  than after its cost is paid, so an ability cancelled mid-payment is still
  logged. It also means the activation line comes before its consequences,
  which is the right way round for a reader.

  Basic abilities say nothing: fight, evade, investigate, move and engage are
  every action a player takes, and the log already covers them better further
  down -- as the test they provoke, or the movement they cause. "Daisy Walker
  activates an ability on the Attic" for walking into the Attic is noise. -}
  UseAbility iid ab _ | not ab.basic && notPlayerAbilityIndex ab.index -> Just do
    who <- investigatorRefFor iid
    mSource <- sourceRefFor ab.source
    pure do
      src <- mSource
      -- Qualified: 'Arkham.Ability.Type' exports an @abilityType@ of its own
      -- (the field of the wrapper constructors), and 'Ability' has no HasField
      -- for this one.
      (key, extra) <- abilityPhrasing (Ab.abilityType ab)
      pure $ action [ikeyPart key (["investigator" ~> who, "source" ~> src] <> extra)]
  -- A location turning face up. The ref is built AFTER the narrator runs on a
  -- still-unrevealed location, so it would draw its back; name it explicitly.
  RevealLocation _ lid -> Just do
    loc <- locationRefFor lid
    mCard <- fieldMay LocationCard lid
    let entry = mechanic [ikeyPart "log.locationRevealed" ["location" ~> loc {logRefFaceDown = False}]]
    -- Opens the block the reveal's clues and triggers land in; see
    -- 'runPreGameMessage'. No card means no block, not no line.
    pure $ Just $ maybe entry (\card -> opensGroup (cardBlockKey (toCardId card)) entry) mCard
  -- Clues moving on and off the board, which is the scenario's clock.
  PlaceClues _ target n | n > 0 -> Just (renderTokens "log.placesClues" target n)
  RemoveClues _ target n | n > 0 -> Just (renderTokens "log.removesClues" target n)
  InvestigatorPlaceCluesOnLocation iid _ n | n > 0 -> Just do
    who <- investigatorRefFor iid
    pure
      $ Just
      $ mechanic [ikeyPart "log.placesCluesOnLocation" ["investigator" ~> who, "count" ~> n]]
  Surge iid source -> Just do
    who <- investigatorRefFor iid
    mSource <- sourceRefFor source
    pure $ Just $ mechanic $ pure $ case mSource of
      Just src -> ikeyPart "log.surgeFrom" ["investigator" ~> who, "source" ~> src]
      Nothing -> ikeyPart "log.surge" ["investigator" ~> who]
  ShuffleDiscardBackIn iid -> Just do
    who <- investigatorRefFor iid
    pure $ Just $ notice [ikeyPart "log.shufflesDiscardBackIn" ["investigator" ~> who]]
  SearchFound iid _ _ cards | notNull cards -> Just do
    who <- investigatorRefFor iid
    mPlayer <- fieldMay InvestigatorPlayerId iid
    let
      entry =
        mechanic
          [ ikeyPart
              "log.searchFound"
              [ "investigator" ~> who
              , "cards" ~> LogList (map toLogPart cards)
              , "count" ~> length cards
              ]
          ]
    pure $ Just $ maybe entry (`forPlayer` entry) mPlayer
  {- The campaign log, which is the only part of a scenario that outlives it.

  The key is rendered by the client, which already has every campaign-log key
  under @campaignLog.*@ for the campaign-log screen. Sending the key rather than
  @ToGameLoggerFormat@'s text keeps it localized and keeps the brace DSL -- which
  that instance still emits for a few keys -- out of a structured entry. -}
  -- Qualified: 'Record' is also a 'LogKind', and both are in scope here.
  Msg.Record key -> Just (pure $ Just $ recordEntry "log.recorded" key)
  CrossOutRecord key -> Just (pure $ Just $ recordEntry "log.crossedOut" key)
  {- A count supersedes the one before it rather than stacking.

  "Chasing the Stranger (1)" then "(2)" is one fact changing, not two things
  that happened, so the earlier line is retracted as the new one is written. -}
  RecordCount key n -> Just do
    let tag = "recordCount:" <> tshow key
    retractLog tag
    pure
      $ Just
      $ tagged tag
      $ record
        [ikeyPart "log.recordedCount" ["entry" ~> campaignLogKeyPart key, "count" ~> n]]
  -- Trauma, which outlives the scenario and so is worth its own line.
  SufferTrauma iid physical mental | physical > 0 || mental > 0 -> Just do
    who <- investigatorRefFor iid
    let (key, extra)
          | physical > 0 && mental > 0 =
              ("log.sufferTraumaBoth", ["physical" ~> physical, "mental" ~> mental])
          | physical > 0 = ("log.sufferPhysicalTrauma", ["count" ~> physical])
          | otherwise = ("log.sufferMentalTrauma", ["count" ~> mental])
    pure $ Just $ record [ikeyPart key (("investigator" ~> who) : extra)]
  TakeControlOfAsset iid aid -> Just do
    who <- investigatorRefFor iid
    asset <- assetRefFor aid
    pure
      $ Just
      $ mechanic [ikeyPart "log.takesControlOf" ["investigator" ~> who, "asset" ~> asset]]
  RemoveFromGame target -> Just do
    mTarget <- targetRefFor target
    pure $ flip fmap mTarget \t -> notice [ikeyPart "log.removedFromGame" ["target" ~> t]]
  -- \* Leaving the scenario
  InvestigatorMessage (InvestigatorResigned_ iid) -> Just do
    who <- investigatorRefFor iid
    pure $ Just $ mechanic [ikeyPart "log.resigns" ["investigator" ~> who]]
  InvestigatorMessage (InvestigatorEliminated_ iid) -> Just do
    who <- investigatorRefFor iid
    pure $ Just $ mechanic [ikeyPart "log.eliminated" ["investigator" ~> who]]
  GainXP iid _ n | n > 0 -> Just do
    who <- investigatorRefFor iid
    pure $ Just $ record [ikeyPart "log.gainsXp" ["investigator" ~> who, "count" ~> n]]
  {- The scenario's ending.

  Guarded on @inResolution@ because the narrator sees this message TWICE: the
  first pass clears the queue and re-pushes the message so the end-of-game
  window can go in front of it (@Arkham.Scenario@), setting @inResolution@ on
  the way. Only the second pass is the resolution actually happening. -}
  ScenarioResolution res -> Just do
    inResolution <- getInResolution
    pure
      $ guard inResolution
      $> structure
        ( pure $ case res of
            Resolution n -> ikeyPart "log.resolution" ["count" ~> n]
            NoResolution -> ikeyPart "log.noResolution" []
        )
  -- \* The decks that end the scenario
  {- Act and agenda advancement.

  Both messages arrive __twice__ per advance: the runner flips the card to its
  back and re-pushes the same message inside a @chooseOne@, which then resolves
  it (@Agenda/Runner.hs:59-61@, @Act/Runner.hs:80@). Narrating the constructor
  alone logs every advance twice, which it did.

  The narrator runs before the message executes, so the card's current side
  tells the two apart: the first arrives face up, the second already flipped. -}
  AdvanceAct aid _ _ -> Just do
    -- Front sides are the odd letters; B/D/… are the backs.
    front <- maybe False ((`elem` [A.A, A.C, A.E, A.G]) . A.actSide) <$> fieldMay ActSequence aid
    if not front
      then pure Nothing
      else do
        ref <- actRefFor aid
        pure $ Just $ structure [ikeyPart "log.actAdvances" ["act" ~> ref]]
  AdvanceAgendaBy aid _ -> Just do
    mSeq <- fieldMay AgendaSequence aid
    let unflipped = maybe False ((`elem` [AS.A, AS.C]) . AS.agendaSide) mSeq
    if not unflipped
      then pure Nothing
      else do
        ref <- agendaRefFor aid
        pure $ Just $ structure [ikeyPart "log.agendaAdvances" ["agenda" ~> ref]]
  _ -> Nothing

-- | "Ghoul Priest attacks Daisy Walker." A massive attack names everyone it hits.
renderEnemyAttack :: HasGame m => EnemyAttackDetails -> m (Maybe LogEntry)
renderEnemyAttack details = do
  enemy <- enemyRefFor details.enemy
  let targets = case details.target of
        SingleAttackTarget t -> [t]
        MassiveAttackTargets ts -> ts
  refs <- catMaybes <$> traverse targetRefFor targets
  pure $ case refs of
    [] -> Nothing
    _ ->
      Just
        $ mechanic
          [ikeyPart "log.enemyAttacks" ["enemy" ~> enemy, "target" ~> LogList (map toLogPart refs)]]

{- | "Daisy Walker heals 2 damage."

No chip for the target means something the reader has no name for was healed;
saying nothing beats printing a uuid, same rule as damage.
-}
renderHealed :: HasGame m => Text -> Target -> Int -> m (Maybe LogEntry)
renderHealed key target n = do
  mTarget <- targetRefFor target
  pure $ flip fmap mTarget \t -> mechanic [ikeyPart key ["target" ~> t, "count" ~> n]]

{- | How an ability reads, by what kind it is.

A paid ability is "activated"; anything that fires off a window is "triggered",
and carries the symbol the card prints so the line matches what the player is
looking at. 'Nothing' means say nothing at all.

'SilentForcedAbility' is deliberately silent: its whole purpose is an effect
whose card does /not/ print "Forced", so announcing one would tell the player
something untrue about their own card -- the same reason the UI does not prompt
for it.
-}
abilityPhrasing :: AbilityType -> Maybe (Text, [(Text, LogPart)])
abilityPhrasing = \case
  ActionAbility {} -> Just ("log.activatesAbility", [])
  AbilityEffect {} -> Just ("log.activatesAbility", [])
  ServitorAbility {} -> Just ("log.activatesAbility", [])
  FastAbility' {} -> Just ("log.triggersAbility", ["symbol" ~> LogIcon "fast"])
  ReactionAbility {} -> Just ("log.triggersAbility", ["symbol" ~> LogIcon "reaction"])
  ConstantReaction {} -> Just ("log.triggersAbility", ["symbol" ~> LogIcon "reaction"])
  CustomizationReaction {} -> Just ("log.triggersAbility", ["symbol" ~> LogIcon "reaction"])
  ForcedAbility {} -> Just ("log.triggersForcedAbility", [])
  ForcedAbilityWithCost {} -> Just ("log.triggersForcedAbility", [])
  Haunted -> Just ("log.triggersHauntedAbility", [])
  -- Wrappers; the kind that matters is inside.
  DelayedAbility inner -> abilityPhrasing inner
  Objective inner -> abilityPhrasing inner
  ForcedWhen _ inner -> abilityPhrasing inner
  SilentForcedAbility {} -> Nothing
  Cosmos -> Nothing
  ConstantAbility -> Nothing

{- | "2 clues are placed on the Study".

The source is deliberately not named: for clues and doom it is almost always
the act, the agenda or the scenario itself, which tells a reader nothing they
cannot already see.
-}
renderTokens :: HasGame m => Text -> Target -> Int -> m (Maybe LogEntry)
renderTokens key target n = do
  mTarget <- targetRefFor target
  pure $ flip fmap mTarget \t -> mechanic [ikeyPart key ["target" ~> t, "count" ~> n]]

{- | How to say a surcharge, for the costs that have wording.

Silent for the rest: an unexplained extra cost is worse than nothing, because it
sends the reader hunting for something the log never said.
-}
surchargeWording :: Cost -> Maybe (Text, Int)
surchargeWording = \case
  ActionCost n -> Just ("log.actionSurcharge", n)
  ResourceCost n -> Just ("log.resourceSurcharge", n)
  _ -> Nothing

-- | A campaign-log key, for the client to name. See 'LogCampaignKey'.
campaignLogKeyPart :: CampaignLogKey -> LogPart
campaignLogKeyPart = LogCampaignKey . toJSON

recordEntry :: Text -> CampaignLogKey -> LogEntry
recordEntry key k = record [ikeyPart key ["entry" ~> campaignLogKeyPart k]]

-- | The heading for a phase.
phaseKey :: Phase -> Text
phaseKey = \case
  MythosPhase -> "log.phase.mythos"
  InvestigationPhase -> "log.phase.investigation"
  EnemyPhase -> "log.phase.enemy"
  UpkeepPhase -> "log.phase.upkeep"
  ResolutionPhase -> "log.phase.resolution"
  CampaignPhase -> "log.phase.campaign"

{- | "Daisy Walker takes 2 damage and 1 horror".

Three shapes so the sentence never carries a zero: both, damage only, horror
only.
-}
renderAssignedDamage :: HasGame m => Target -> Source -> Int -> Int -> m (Maybe LogEntry)
renderAssignedDamage target source damage horror = do
  mTarget <- targetRefFor target
  -- "from the Ghoul Minion". Dropped rather than guessed when the source has no
  -- name a reader would recognise -- an upkeep step, the scenario itself -- for
  -- the same reason the target is.
  mSource <- sourceRefFor source
  -- No chip for the target means something the reader has no name for took the
  -- hit; saying nothing beats printing a uuid.
  pure $ flip fmap mTarget \t ->
    let (key, extra)
          | damage > 0 && horror > 0 =
              ("log.takesDamageAndHorror", ["damage" ~> damage, "horror" ~> horror])
          | damage > 0 = ("log.takesDamage", ["count" ~> damage])
          | otherwise = ("log.takesHorror", ["count" ~> horror])
        (key', extra') = case mSource of
          Just src -> (key <> "From", ("source" ~> src) : extra)
          Nothing -> (key, extra)
     in mechanic [ikeyPart key' (("target" ~> t) : extra')]

{- | "Ghoul Priest spawns at the Study".
| "Daisy Walker draws Deep One Ambusher".
-}
drawsEncounter :: HasGame m => InvestigatorId -> EncounterCard -> m (Maybe LogEntry)
drawsEncounter iid card = do
  who <- investigatorRefFor iid
  pure
    $ Just
    $ opensGroup (cardBlockKey (toCardId card))
    $ mechanic [ikeyPart "log.drawsEncounter" ["investigator" ~> who, "card" ~> toCard card]]

{- | "Choosing where Dancing Rats spawns", tagged so the spawn takes it back
out again rather than leaving the question standing above its own answer.
-}
renderSpawnChoice :: HasGame m => EnemyId -> LocationMatcher -> m (Maybe LogEntry)
renderSpawnChoice eid matcher = do
  locations <- select matcher
  if length locations < 2
    then pure Nothing
    else do
      enemy <- enemyRefFor eid
      pure
        $ Just
        $ tagged (spawnChoiceLogTag eid)
        $ notice [ikeyPart "log.choosingSpawnLocation" ["enemy" ~> enemy]]

renderSpawned :: (HasGame m, HasGameLogger m) => SpawnDetails -> m (Maybe LogEntry)
renderSpawned details = do
  -- The spawn replaces the prompt that asked for it rather than following it.
  -- A no-op when nothing asked, which is most spawns.
  retractLog (spawnChoiceLogTag details.enemy)
  enemy <- enemyRefFor details.enemy
  mLocation <- join <$> fieldMay EnemyLocation details.enemy
  locRef <- traverse locationRefFor mLocation
  pure $ Just $ mechanic $ pure $ case locRef of
    Just loc -> ikeyPart "log.enemySpawnedAt" ["enemy" ~> enemy, "location" ~> loc]
    Nothing -> ikeyPart "log.enemySpawned" ["enemy" ~> enemy]

-- | "Ghoul Priest is defeated by Daisy Walker".
renderDefeated :: HasGame m => Target -> Source -> m (Maybe LogEntry)
renderDefeated target source = do
  mTarget <- targetRefFor target
  mSource <- sourceRefFor source
  -- An investigator going down is bad; anything else going down is the point.
  let tone = case target of
        InvestigatorTarget _ -> Bad
        _ -> Good
  pure $ flip fmap mTarget \t -> toned tone $ mechanic $ pure $ case mSource of
    Just src -> ikeyPart "log.defeatedBy" ["target" ~> t, "source" ~> src]
    Nothing -> ikeyPart "log.defeated" ["target" ~> t]

{- | "Daisy Walker passes [intellect] by 2 investigating the Study".

The target says /what the test was against/, which is most of what makes the
line worth reading. It comes off the message, so no extra bookkeeping -- only a
lookup to turn the id into a chip.
-}
renderSkillTestResult
  :: (HasGame m, HasGameLogger m)
  => InvestigatorId
  -> Maybe Action
  -> Target
  -> SkillTestType
  -> Bool
  -> Int
  -> m (Maybe LogEntry)
renderSkillTestResult iid mAction target sType success n = do
  -- A test "against" the investigator taking it reads as noise -- "Daisy Walker
  -- fails by 2 against Daisy Walker". Drop the target in that case and use the
  -- plain shape.
  mRef <- if target == InvestigatorTarget iid then pure Nothing else targetRefFor target
  let
    -- Three shapes, so no sentence ever has a hole in it: an action and a
    -- target ("investigating the Study"); a target but no action, which is what
    -- a treachery's revelation test looks like; or neither.
    (key, extra) = case (mRef, mAction) of
      (Just ref, Just a) ->
        ( if success then "log.skillTestPassedAgainst" else "log.skillTestFailedAgainst"
        , -- "verb", not "action": {action} is an Arkham icon placeholder, and the
          -- client escapes it to a literal glyph before vue-i18n ever sees it, so
          -- a variable by that name is silently never substituted.
          [("verb", ikeyPart ("log.action." <> actionKey a) []), ("target", toLogPart ref)]
        )
      (Just ref, Nothing) ->
        ( if success then "log.skillTestPassedVs" else "log.skillTestFailedVs"
        , [("target", toLogPart ref)]
        )
      (Nothing, _) ->
        (if success then "log.skillTestPassed" else "log.skillTestFailed", [])
  who <- investigatorRefFor iid
  stats <- skillTestStats
  mKey <- openSkillTestKey
  -- The arithmetic in full, inside the block. The band carries only the totals,
  -- which is what a reader scanning the log wants; this is what they open the
  -- block to check.
  for_ mKey \k -> traverse_ (sendLog . inGroupOf k) =<< skillTestBreakdown
  let
    entry =
      toned (if success then Good else Bad)
        $ test
        $ ikeyPart
          key
          ( [ "investigator" ~> who
            , "skill" ~> renderSkillTestType sType
            , "by" ~> n
            ]
              <> extra
          )
        -- The arithmetic belongs in the BODY, not among the children: the
        -- client collapses a test down to its band, and the numbers are the
        -- half of that band worth keeping when the rest is folded away.
        : stats
  -- Closes the block the test opened: the band it collapses to. With no group
  -- -- a result arriving with no test installed -- it stands on its own rather
  -- than being lost.
  pure $ Just $ maybe entry (`closesGroup` entry) mKey

-- | What a commit's log line is tagged with, so an uncommit can retract it.
commitLogTag :: Card -> Text
commitLogTag card = "commit:" <> tshow (toCardId card)

{- | Names the prompt asking where an enemy spawns, so the spawn can take it
back out. One per enemy: nothing chooses two spawn points for the same enemy.
-}
spawnChoiceLogTag :: EnemyId -> Text
spawnChoiceLogTag eid = "spawnChoice:" <> idText eid

{- | The key the open test's block is filed under.

'Nothing' when no test is running, which is how every narration that can happen
either inside or outside a test decides where it belongs.
-}
openSkillTestKey :: HasGame m => m (Maybe Text)
openSkillTestKey = fmap skillTestLogKey <$> getSkillTest

{- | The block, written the moment the test begins.

This is the line a reader watches grow: "Daisy Walker is investigating the
Study [intellect] (2)". Everything that happens during the test is attached
underneath it, and the result rewrites this same row rather than adding another
-- which is what 'logEntryKey' is for.

The difficulty is read off the record passed in rather than through
'getSkillTestDifficulty', because the test is not installed yet when this runs.
-}
renderSkillTestOpening :: HasGame m => SkillTest -> m (Maybe LogEntry)
renderSkillTestOpening st = do
  who <- investigatorRefFor st.investigator
  difficulty <- getModifiedSkillTestDifficulty st
  mRef <-
    if skillTestTarget st == InvestigatorTarget st.investigator
      then pure Nothing
      else targetRefFor (skillTestTarget st)
  let
    (key, extra) = case (mRef, st.action) of
      (Just ref, Just a) ->
        ( "log.testOpensAgainst"
        , [("verb", ikeyPart ("log.action." <> actionKey a) []), ("target", toLogPart ref)]
        )
      (Just ref, Nothing) -> ("log.testOpensVs", [("target", toLogPart ref)])
      (Nothing, _) -> ("log.testOpens", [])
  pure
    $ Just
    $ opensGroup (skillTestLogKey st)
    $ test
      [ ikeyPart key
          $ [ "investigator" ~> who
            , "skill" ~> renderSkillTestType (skillTestType st)
            , "difficulty" ~> difficulty
            ]
          <> extra
      ]

{- | The arithmetic that explains the result, for the block's band.

Dropped rather than guessed when the difficulty cannot be calculated, so the
band is simply shorter rather than carrying a hole.
-}

{- | Every number that went into the result, as one line.

The engine already assembles exactly this to decide the outcome
('SkillTestResultsData'), so the log reports what the test actually used rather
than recomputing it and risking a different answer.

Icons and tokens are deltas, because both can be negative and a reader is
adding them up: "Skill 3, +2 from icons, -1 from tokens -- 4 vs 2".
-}
skillTestBreakdown :: HasGame m => m (Maybe LogEntry)
skillTestBreakdown = fmap (fmap render) skillTestResults
 where
  render r =
    notice
      [ ikeyPart
          "log.testBreakdown"
          [ "skill" ~> skillTestResultsSkillValue r
          , "icons" ~> delta (skillTestResultsIconValue r)
          , "tokens" ~> delta (skillTestResultsChaosTokensValue r)
          , "total" ~> adjustedSkillValue r
          , "difficulty" ~> skillTestResultsDifficulty r
          ]
      ]

skillTestResults :: HasGame m => m (Maybe SkillTestResultsData)
skillTestResults = getSkillTest >>= traverse calculateSkillTestResultsData

{- | What the test was finally worth: the modified skill plus the icons
committed to it plus the chaos tokens, floored at zero, which is the number the
difficulty was actually compared against.

NOT 'getSkillTestModifiedSkillValue', which is the skill alone -- the band said
"5 vs difficulty 4" while the test was decided on a different number.
-}
adjustedSkillValue :: SkillTestResultsData -> Int
adjustedSkillValue r =
  max 0
    $ skillTestResultsSkillValue r
    + skillTestResultsIconValue r
    + skillTestResultsChaosTokensValue r

skillTestStats :: HasGame m => m [LogPart]
skillTestStats =
  skillTestResults <&> \case
    Just r ->
      [ ikeyPart
          "log.testArithmetic"
          ["value" ~> adjustedSkillValue r, "difficulty" ~> skillTestResultsDifficulty r]
      ]
    Nothing -> []

{- | The wire name for an action, matching the keys in @log.json@. Every
'Action' constructor is a single word, so lowering the first letter is the
whole rule.
-}
actionKey :: Action -> Text
actionKey a = let t = tshow a in T.toLower (T.take 1 t) <> T.drop 1 t

{- | The icon a test is against. A multi-skill or base-value test has no single
icon, so it renders as its own word rather than a wrong one.
-}
renderSkillTestType :: SkillTestType -> LogPart
renderSkillTestType = \case
  SkillSkillTest sk -> toLogPart sk
  AndSkillTest sks -> LogList (map toLogPart sks)
  ResourceSkillTest -> ikeyPart "log.resources" []
  BaseValueSkillTest {} -> ikeyPart "log.baseValue" []
