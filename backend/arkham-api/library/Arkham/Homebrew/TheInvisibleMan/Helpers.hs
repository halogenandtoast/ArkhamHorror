{- | Shared rules for The Invisible Man.

Three things run through the whole scenario.

Griffin's three stats are printed @?@, so by default his fight and evade are 0
and he has no health at all. Acts 1d and 1f are what pin fight and evade to 3,
and the memory each one records is also what the story cards mean by "if
Griffin's fight/evade value is a set number".

Every attack against Griffin draws the top card of the Invisible Combat deck
and every evasion draws from the Invisible Escape deck. A drawn card is read,
resolves, and shuffles itself back in, so the two decks are stable stacks
rather than something that runs out.

A "lead" is 1 horror on a location. Three locations can place one, and the
Run-Down Cabin counts them all to find Griffin's notes.
-}
module Arkham.Homebrew.TheInvisibleMan.Helpers where

import Arkham.Calculation (GameCalculation (GameValueCalculation, LocationMatcherFieldCalculation))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types (Enemy, Field (EnemyEvadeActual, EnemyFightActual))
import Arkham.GameEnv (getCurrentBatchId)
import Arkham.GameValue (GameValue (ValueUnknown))
import Arkham.Helpers.Calculation (calculate)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Helpers.SkillTest (getSkillTest, skillTestMatches)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheInvisibleMan.ScenarioDeckKeys
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (Field (LocationHorror))
import Arkham.Matcher
import Arkham.Message (
  Message (RemoveCardFromScenarioDeck, ShuffleCardsIntoDeck, StoryMessage),
  pattern MoveToward,
 )
import Arkham.Message.Lifted (
  cancelBatch,
  checkAfter,
  damageModifier,
  disengageEnemy,
  focusCards,
  initiateEnemyAttack,
  placeTokens,
  removeTokens,
  roundModifiers,
  skillTestModifier,
  toDiscard,
 )
import Arkham.Message.Lifted.Choose (chooseOrRunOneM, targets)
import Arkham.Message.Lifted.Move (enemyMoveTo)
import Arkham.Message.Lifted.Queue
import Arkham.Message.Story (StoryMessage (ReadStory))
import Arkham.Prelude
import Arkham.Projection (field)
import Arkham.Scenario.Deck (ScenarioDeckKey)
import Arkham.ScenarioLogKey (ScenarioLogKey (..))
import Arkham.SkillTest.Step (SkillTestStep (RevealChaosTokenStep))
import Arkham.Source (Source (ActSource), Sourceable, toSource)
import Arkham.Target (Targetable, toTarget)
import Arkham.Treachery.Types (TreacheryAttrs)
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

-- * Memories

memory :: Text -> ScenarioLogKey
memory = HomebrewScenarioLogKey . ("theInvisibleMan." <>)

-- | "Remember that you \"found Griffin's notes.\"" -- the Run-Down Cabin.
foundGriffinsNotes :: ScenarioLogKey
foundGriffinsNotes = memory "FoundGriffinsNotes"

-- * Griffin

griffin :: EnemyMatcher
griffin = enemyIs Enemies.griffin

{- | "Griffin moves one location toward you." -- the cultist token. He takes a
single step along the shortest path, which is what 'MoveToward' does.
-}
moveGriffinToward :: ReverseQueue m => InvestigatorId -> m ()
moveGriffinToward iid =
  selectOne griffin >>= traverse_ \eid ->
    push $ MoveToward (toTarget eid) (locationWithInvestigator iid)

-- | "If Griffin is ready and at your location, he attacks you." -- the tablet token.
griffinAttacksIfReadyAt
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
griffinAttacksIfReadyAt source iid = do
  mGriffin <- selectOne $ griffin <> ReadyEnemy <> at_ (locationWithInvestigator iid)
  for_ mGriffin \eid -> initiateEnemyAttack eid source iid

{- | "...you must choose a location with a Sheldon Supplier (whether or not
there are investigators there), if able." -- Sheldon Supplier. He marks his own
location with this, so 'griffinMoves' reads the mark instead of naming him.
-}
griffinMustEnter :: ModifierType
griffinMustEnter = ScenarioModifier "theInvisibleMan.griffinMustEnter"

{- | "...and moves to a connecting location (without investigators, if
possible)." Every move Griffin's forced effect makes goes through here, so
Sheldon Supplier's clause has one place to narrow: a marked connecting location
is mandatory, and it overrides the preference for an empty one.
-}
griffinMoves :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> m ()
griffinMoves source eid = do
  let connecting = connectedFrom (locationWithEnemy eid)
  marked <- select $ connecting <> LocationWithModifier griffinMustEnter
  unoccupied <- select $ connecting <> LocationWithoutInvestigators
  occupied <- select connecting
  let
    destinations
      | notNull marked = marked
      | notNull unoccupied = unoccupied
      | otherwise = occupied
  when (notNull destinations) do
    lead <- getLead
    chooseOrRunOneM lead $ targets destinations $ enemyMoveTo source eid

-- | "After you enter Griffin's location or vice versa".
enteredGriffinsLocation :: WindowMatcher
enteredGriffinsLocation =
  oneOf
    [ Moves #after You AnySource Anywhere (LocationWithEnemy griffin)
    , EnemyEntersYourLocation #after griffin
    ]

{- | "remove 1 resource from <this>. If it has no resources on it, discard it."
-- how Malicious Laughter and Out of Nowhere both count down.
-}
removeResourceOrDiscard :: ReverseQueue m => Source -> TreacheryAttrs -> m ()
removeResourceOrDiscard source attrs = do
  removeTokens source attrs #resource 1
  when (attrs.resources <= 1) $ toDiscard source attrs

-- * Leads

{- | "...leads among all locations." Horror on a location is the only thing that
makes a lead, so counting leads is counting that horror.

A calculation rather than a query, because the Run-Down Cabin needs it as
ability criteria and the two must not be able to disagree.
-}
leadCount :: GameCalculation
leadCount = LocationMatcherFieldCalculation Anywhere LocationHorror

getLeadCount :: HasGame m => m Int
getLeadCount = calculate leadCount

-- | "Place 1 horror on this location, as a lead."
placeLead
  :: (ReverseQueue m, Sourceable source, Targetable location) => source -> location -> m ()
placeLead source location = placeTokens source location #horror 1

-- * The Invisible Combat and Invisible Escape decks

{- | "Draw the top card of the Invisible Combat deck." The card leaves the deck
and is read by the investigator whose attack or evasion provoked it; its own
last line shuffles it back in.

The draw raises a real 'Window.DrawCard' window naming the scenario deck it came
off, which is the window the Meltwater Stream reacts to -- the same position the
engine's own story draw puts it in ('Do' @InvestigatorDrewEncounterCardFrom@,
which pushes the after-draw window ahead of the @ReadStory@). Nothing else can
see it: every other @DrawCard@ reaction in the game pins its card matcher to a
treachery, an encounter card or a trait these stories do not carry.

The card is focused across the window, because the only ability that reacts to
it asks whether to throw the card away and no one can answer that without
seeing it.
-}
drawInvisibleCard :: ReverseQueue m => ScenarioDeckKey -> InvestigatorId -> m ()
drawInvisibleCard key iid =
  getScenarioDeck key >>= \case
    [] -> pure ()
    card : _ -> do
      push $ RemoveCardFromScenarioDeck key card
      focusCards [card] $ checkAfter $ Window.DrawCard iid card (Deck.ScenarioDeckByKey key)
      readStory iid card (toCardDef card)

-- | "After you draw a card from the Invisible Combat or Invisible Escape deck".
drewInvisibleCard :: WindowMatcher
drewInvisibleCard =
  DrawCard #after You (basic AnyCard)
    $ DeckOneOf
      [ DeckIs (Deck.ScenarioDeckByKey InvisibleCombatDeck)
      , DeckIs (Deck.ScenarioDeckByKey InvisibleEscapeDeck)
      ]

{- | The card drawn and the deck it came off, read back out of
'drewInvisibleCard'. The deck comes from the window rather than from the card's
trait so that the card goes back where it actually came from.
-}
invisibleCardDrawn :: [Window] -> Maybe (Card, ScenarioDeckKey)
invisibleCardDrawn ws =
  listToMaybe
    [(card, key) | (windowType -> Window.DrawCard _ card (Deck.ScenarioDeckByKey key)) <- ws]

{- | The read that 'drawInvisibleCard' queued behind its window, for the
Meltwater Stream's "cancel that card's effects": the story is never created, so
none of its text resolves and nothing shuffles it back on its own.
-}
isInvisibleCardRead :: IsCard card => card -> Message -> Bool
isInvisibleCardRead card = \case
  StoryMessage (ReadStory _ c _ _) -> toCardCode c == toCardCode card
  _ -> False

drawInvisibleCombat :: ReverseQueue m => InvestigatorId -> m ()
drawInvisibleCombat = drawInvisibleCard InvisibleCombatDeck

drawInvisibleEscape :: ReverseQueue m => InvestigatorId -> m ()
drawInvisibleEscape = drawInvisibleCard InvisibleEscapeDeck

{- | "Shuffle <this card> back into the Invisible Combat deck." Every card in
both decks ends this way, which is what keeps them from running dry.
-}
shuffleBackInto :: (ReverseQueue m, IsCard card) => ScenarioDeckKey -> card -> m ()
shuffleBackInto key card =
  push $ ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey key) [toCard card]

shuffleBackIntoCombat :: (ReverseQueue m, IsCard card) => card -> m ()
shuffleBackIntoCombat = shuffleBackInto InvisibleCombatDeck

shuffleBackIntoEscape :: (ReverseQueue m, IsCard card) => card -> m ()
shuffleBackIntoEscape = shuffleBackInto InvisibleEscapeDeck

{- | Acts 2d and 2f read their own options off the deck: an option is still
open exactly when the deck does or does not already hold the card it names.
-}
inScenarioDeck :: HasGame m => ScenarioDeckKey -> CardDef -> m Bool
inScenarioDeck key def = any (`cardMatch` cardIs def) <$> getScenarioDeck key

{- | "Search the Invisible Combat deck for <card> and remove it from the
game." Dropping it from the deck is the whole of it -- these cards live
nowhere else.
-}
removeFromScenarioDeck :: ReverseQueue m => ScenarioDeckKey -> CardDef -> m ()
removeFromScenarioDeck key def = do
  cards <- getScenarioDeck key
  for_ (find (`cardMatch` cardIs def) cards) $ push . RemoveCardFromScenarioDeck key

-- * What the Invisible Combat and Invisible Escape cards answer

{- | What the agenda's __Forced__ trigger interrupted.

Each of its two arms is a different kind of event and the story cards have to
treat them differently. An attack or an evasion /attempt/ has a skill test
standing in front of it whose result can still be set; a card or ability that
deals damage to or evades Griffin automatically has no test at all, and has to
be stopped at the effect that is already pending.

A test that has reached ST.3 is not an attempt being declared -- it is the
damage a successful fight is dealing -- so it reads as 'AutomaticEffect'
rather than as a test still to be failed.
-}
data InvisibleAttempt
  = -- | An attack or evasion attempt, declared and not yet made.
    AttemptDeclared SkillTestId
  | -- | No test: a card or ability doing it by itself.
    AutomaticEffect
  deriving stock (Show, Eq)

attemptAgainstGriffin
  :: (HasGame m, Sourceable source)
  => source -> InvestigatorId -> SkillTestMatcher -> m InvisibleAttempt
attemptAgainstGriffin source iid matcher =
  getSkillTest >>= \case
    Just st | st.step < RevealChaosTokenStep -> do
      declared <- skillTestMatches iid (toSource source) st matcher
      pure $ if declared then AttemptDeclared st.id else AutomaticEffect
    _ -> pure AutomaticEffect

-- | The Invisible Combat half: "if you are attacking".
attackOnGriffin
  :: (HasGame m, Sourceable source) => source -> InvestigatorId -> m InvisibleAttempt
attackOnGriffin source iid = attemptAgainstGriffin source iid (WhileAttackingAnEnemy griffin)

-- | The Invisible Escape half: "if you are making an evasion attempt".
evasionOfGriffin
  :: (HasGame m, Sourceable source) => source -> InvestigatorId -> m InvisibleAttempt
evasionOfGriffin source iid = attemptAgainstGriffin source iid (WhileEvadingAnEnemy griffin)

{- | "If you are attacking, this attack automatically fails. If you played a
card or activated an ability, cancel each of its effects."

The attempt half is 'SkillTestAutomaticallyFails', which the skill test runner
reads at @TriggerSkillTest@ -- so it has to be set before the test is made,
which is exactly where the agenda's /when/ window sits.

The automatic half caps the damage Griffin is about to take at 0 for the length
of that one damage. Nothing in the engine reaches back into a card that is
already resolving, so what is cancelled is the damage to Griffin, not the rest
of the card.
-}
theAttackFails :: (ReverseQueue m, Sourceable source) => source -> InvisibleAttempt -> m ()
theAttackFails source = \case
  AttemptDeclared sid -> skillTestModifier sid source sid SkillTestAutomaticallyFails
  AutomaticEffect ->
    withGriffin \eid -> damageModifier source eid (MaxDamageTaken AnyDamageEffect 0)

{- | "If you are making an evasion attempt, this test automatically fails. If
you played a card or activated an ability, cancel each of its effects."

The automatic half cancels the @EnemyWouldBeEvaded@ batch the enemy runner
wraps around every evasion, which is what leaves Griffin engaged and ready. The
batch is read off the game rather than off a window list because a story
resolves from 'readStory' and never sees the windows it was drawn in.
-}
theEvasionFails :: (ReverseQueue m, Sourceable source) => source -> InvisibleAttempt -> m ()
theEvasionFails source = \case
  AttemptDeclared sid -> skillTestModifier sid source sid SkillTestAutomaticallyFails
  AutomaticEffect -> getCurrentBatchId >>= traverse_ cancelBatch

{- | "Griffin gets +1 fight while resolving its effects." Only an attempt has a
value to beat, so this is scoped to the test and says nothing to a card or
ability that needs no test.
-}
griffinGetsWhileResolving
  :: (ReverseQueue m, Sourceable source) => source -> InvisibleAttempt -> ModifierType -> m ()
griffinGetsWhileResolving source attempt modifier = case attempt of
  AttemptDeclared sid -> withGriffin \eid -> skillTestModifier sid source eid modifier
  AutomaticEffect -> pure ()

-- | "If this effect damages Griffin, deal @n@ additional damage to him."
additionalDamageToGriffin
  :: (ReverseQueue m, Sourceable source) => source -> InvisibleAttempt -> Int -> m ()
additionalDamageToGriffin source attempt n = withGriffin \eid -> case attempt of
  AttemptDeclared sid -> skillTestModifier sid source eid (DamageTaken n)
  AutomaticEffect -> damageModifier source eid (DamageTaken n)

{- | "However, Griffin disengages from you and cannot engage you until the end
of the round. You cannot target Griffin with any attack, card or ability until
the end of the round."

"Target" is not an Arkham keyword, so the second sentence is spelled out of the
permissions the engine does honor: the fight and evade actions, a triggered
ability on him, and damage from one of your own cards. A card that touches him
some other way is not covered -- there is no "cannot be targeted" modifier.
-}
griffinBreaksAwayFrom
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
griffinBreaksAwayFrom source iid = withGriffin \eid -> do
  disengageEnemy iid eid
  roundModifiers
    source
    eid
    [ CannotEngage iid
    , CannotBeDamagedByPlayerSources (SourceUsedBy $ InvestigatorWithId iid)
    ]
  roundModifiers
    source
    iid
    [ CannotFight (be eid)
    , CannotEvade (be eid)
    , CannotTriggerAbilityMatching (AbilityOnEnemy $ be eid)
    ]

-- | "Griffin makes an immediate attack against you."
griffinAttacks :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
griffinAttacks source iid = withGriffin \eid -> initiateEnemyAttack eid source iid

withGriffin :: HasGame m => (EnemyId -> m ()) -> m ()
withGriffin = selectForMaybeM griffin

{- | "If Griffin's fight value is a set number" -- read off Griffin rather than
out of the scenario log, because the value itself is the fact. Acts 1d and 1f
are not "Remember that..." clauses and must not show in the campaign log.

Griffin prints "?", which is a value the engine holds and calculates as 0, so
'EnemyWithFight' (a plain @isJust@) is true from setup and cannot answer this.
The printed value has to be inspected directly.
-}
griffinsFightIsASetNumber :: HasGame m => m Bool
griffinsFightIsASetNumber = griffinsValueIsASetNumber EnemyFightActual

-- | "If Griffin's evade value is a set number" -- act 1f pinned it to 3.
griffinsEvadeIsASetNumber :: HasGame m => m Bool
griffinsEvadeIsASetNumber = griffinsValueIsASetNumber EnemyEvadeActual

{- | Whether one of Griffin's printed values has stopped being "?". Reads the
@Actual@ field so no modifier can make an unknown value look like a number.
-}
griffinsValueIsASetNumber :: HasGame m => Field Enemy (Maybe GameCalculation) -> m Bool
griffinsValueIsASetNumber fld =
  selectOne griffin >>= \case
    Nothing -> pure False
    Just eid -> maybe False isSetNumber <$> field fld eid
 where
  isSetNumber = \case
    GameValueCalculation ValueUnknown -> False
    _ -> True

{- | Whether a source is the given act. An act's id is its card code, so the
two sides of an act share it -- this matches either.
-}
isAct :: CardDef -> Source -> Bool
isAct def = \case
  ActSource aid -> aid == ActId (toCardCode def)
  _ -> False

-- | "If act 2a is in play" -- See Through to the End, the 'a' deck's second act.
act2aInPlay :: HasGame m => m Bool
act2aInPlay = selectAny $ ActWithId (ActId $ toCardCode Acts.seeThroughToTheEnd)

-- * i18n

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theInvisibleMan" a

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "scenario" a
