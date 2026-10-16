{- | Shared rules for Against the Wendigo.

Three of the scenario's "additional rules and clarifications" are general enough
that every card answers to them, so they live here rather than on any one card:

* __The river.__ An investigator standing on a @River@ location cannot move or
  be moved onto another @River@ location by an ordinary move; only Walk Along
  the River and Navigate get them along the water. Hunter enemies are not
  affected. The river links are real connections, so distance, hunters and
  "nearest" queries all work; the restriction is a 'CannotEnter' the scenario
  hands to investigators who are standing on the water (the same shape In Too
  Deep uses for its barricades).

* __Guides.__ Any @Guide@ asset can be passed to another investigator at your
  location at the start of their turn, once per round, and only one Guide may
  answer a given skill test.

* __Navigate.__ One action, once per round, resolved in four steps.
-}
module Arkham.Homebrew.AgainstTheWendigo.Helpers where

import Arkham.Ability
import Arkham.Actions (Actions (SingleAction))
import Arkham.Calculation (GameCalculation (Fixed))
import Arkham.Card (toCardDef)
import Arkham.Card.CardCode (HasCardCode, toCardCode)
import Arkham.Card.CardDef (CardDef)
import Arkham.Card.CardType (CardType (TreacheryType))
import Arkham.ChaosToken
import Arkham.Classes.GameLogger (HasGameLogger)
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.Classes.HasQueue (push, pushAll)
import Arkham.Classes.Query
import Arkham.Direction (Direction (..))
import Arkham.Distance (unDistance)
import Arkham.GameEnv (getDistance)
import Arkham.GameValue (GameValue (ByPlayerCount))
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgainstTheWendigo.Actions (pattern Navigate, pattern WalkAlongTheRiver)
import Arkham.Homebrew.AgainstTheWendigo.ScenarioDeckKeys (pattern StudentsFateDeck)
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Guide)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (LocationAttrs, connectsToL)
import Arkham.Log (ikeyScoped, inGroupOf, notice, opensGroup, sendLog, sendLogs, structure, (~>))
import Arkham.Log qualified as Log
import Arkham.Log.Refs (investigatorRefFor)
import Arkham.Matcher
import Arkham.Message (
  Message (
    CheckAttackOfOpportunity,
    CloseLogBlock,
    MoveWithSkillTest,
    OpenLogBlock,
    RemoveCardFromScenarioDeck,
    StoryMessage,
    Will
  ),
 )
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (forcedMoveTo)
import Arkham.Message.Story (
  StoryMessage (PlaceStory, ResolveStory, ResolvedStory),
  StoryMode (ResolveIt),
 )
import Arkham.Placement (Placement (Global, Unplaced))
import Arkham.Prelude
import Arkham.Projection (field)
import Arkham.SkillType (SkillType (SkillAgility, SkillCombat))
import Arkham.Source
import Arkham.Story.Types qualified as Story
import Arkham.Trait (Trait (River))
import Arkham.Treachery.Types (TreacheryAttrs)

-- * Text

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "againstTheWendigo" a

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "scenario" a

{- | "This location is connected to the River location directly to the East or
West." Every Uncharted location prints it, and the river is the only thing in
the column beside them.
-}
connectsEastOrWest :: LocationAttrs -> LocationAttrs
connectsEastOrWest = connectsToL .~ setFromList [LeftOf, RightOf]

-- | The location an attached treachery sits on, or nothing while it is unattached.
attachedLocation :: TreacheryAttrs -> LocationMatcher
attachedLocation a = maybe Nowhere LocationWithId a.attached.location

{- | "...directly to the East or West", and its North/South counterpart. The
valley is laid out in the grid, so the compass is the grid's, not the
connections' -- the river locations connect North and South as well.
-}
inDirectionsOf :: [Direction] -> TreacheryAttrs -> LocationMatcher
inDirectionsOf ds a = oneOf [LocationInDirection d (attachedLocation a) | d <- ds]

-- * The river

-- | Every location that is on the water. The middle column of the grid.
riverLocation :: LocationMatcher
riverLocation = LocationWithTrait River

{- | Navigate's legal destinations, given where the investigator is standing.

Step 2 forbids revealing a location during the action, so the water has to be
charted before you can set out for it -- which is also why the ability itself
hides until there is somewhere charted to go. Walk Along the River has no such
clause: it is the action that does the charting, one location at a time.
-}
navigateDestinations :: LocationMatcher -> LocationMatcher
navigateDestinations here = riverLocation <> RevealedLocation <> not_ here

{- | "Investigators in River locations cannot move or be moved onto another
River location, unless they perform Walk Along the River or Navigate. Hunter
enemies are not affected."

A constant effect every River location carries rather than a Forced ability --
nothing triggers, so there is nothing to trigger off. Each stretch of water
tells the investigators standing on a *different* one that they cannot enter it.
That is also what exempts the Hunters for free: 'CannotEnter' sits on the
investigator, so an enemy moving along the river never consults it.

Both river actions move with 'forcedMoveTo', which skips the move criteria this
rides on, so they stay free to cross.
-}
riverMovementBan :: HasModifiersM m => LocationAttrs -> m ()
riverMovementBan a =
  modifySelect a (InvestigatorAt $ riverLocation <> not_ (be a)) [CannotEnter a.id]

-- * The two river actions

-- | @{action}{action}@: Walk Along the River.
walkAlongTheRiverAction :: AbilityType
walkAlongTheRiverAction =
  ActionAbility (SingleAction WalkAlongTheRiver) Nothing (ActionCost 2)

-- | @{action}@: Navigate. Once per round, by the scenario's additional rules.
navigateAction :: AbilityType
navigateAction = ActionAbility (SingleAction Navigate) Nothing (ActionCost 1)

{- | Walk Along the River moves you to a /connected/ river location — one step,
no test.
-}
resolveWalkAlongTheRiver
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
resolveWalkAlongTheRiver source iid = do
  destinations <- select $ riverLocation <> connectedFrom (locationWithInvestigator iid)
  chooseOrRunOneM iid $ targets destinations $ forcedMoveTo source iid

{- | Navigate, in the scenario's four printed steps.

Step 4's difficulty is @1 + X@, where X is how many river locations the
investigator crossed, which is why the move is resolved before the test is
built. Rapid raises that difficulty and the Guide assets lower the effective
test, both by reacting to the test this starts.
-}
resolveNavigate
  :: (HasGameLogger m, ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
resolveNavigate (toSource -> source) iid = do
  key <- beginNavigateLog iid

  {- Step 1: the attack of opportunity taking the action provokes, written out as
  a step of its own.

  The ability carries 'noAOO' so the engine does not also provoke one while
  paying for it. That is not a mechanical change -- it is the same single attack
  -- but it is the only way to get it inside step 1: the cost is paid behind an
  ask, and anything queued before that ask resolves behind whatever the answer
  injects, so no block opened in advance is still open by the time the engine's
  own attack narrates.

  Both halves, in this order: 'Will' is what arms each enemy's
  @attackOfOpportunityFlagged@, reading its engagement and exhaustion then, and
  the bare message does nothing at all to an enemy that was never armed. -}
  openNavigateStep key 1
  pushAll
    [Will (CheckAttackOfOpportunity iid False Nothing), CheckAttackOfOpportunity iid False Nothing]
  closeNavigateStep key 1

  -- Step 2: EVERY enemy disengages -- the printed step says every enemy, not
  -- every ready one -- and nothing may re-engage during the action.
  openNavigateStep key 2
  engaged <- select $ enemyEngagedWith iid
  for_ engaged disengageFromAll
  roundModifier source iid CannotBeEngaged
  closeNavigateStep key 2
  -- Step 2 also forbids revealing a location during the action. Step 3 moves
  -- straight to the destination, so nothing in between can be revealed, and
  -- 'navigateDestinations' keeps the destination itself off the table unless it
  -- is already charted.

  -- Steps 3 and 4: move, then test against the distance travelled. Both hang
  -- off the destination ask, so each closes its own block from in there.
  start <- getMaybeLocation iid
  destinations <- select $ navigateDestinations (locationWithInvestigator iid)
  openNavigateStep key 3
  chooseOrRunOneM iid $ targets destinations \lid -> do
    mdistance <- maybe (pure Nothing) (`getDistance` lid) start
    let steps = maybe 1 (min 3 . max 1 . unDistance) mdistance
    forcedMoveTo source iid lid
    closeNavigateStep key 3

    openNavigateStep key 4
    sid <- getRandom
    -- "If you fail, take 1 damage, or 2 damage if you fail by 3 or more." Both
    -- riders are created; the amount failed by picks exactly one of them.
    onFailedByEffect sid (atLeast 3) source iid $ assignDamage iid source 2
    onFailedByEffect sid (lessThan 3) source iid $ assignDamage iid source 1
    chooseOneM iid $ skillsLabeled [#combat, #agility] \sk ->
      beginSkillTest sid iid source iid sk (Fixed $ 1 + steps)
    closeNavigateStep key 4

  -- Queued last, so it resolves behind everything the steps queue -- the
  -- destination ask, the move, the skill test -- and the block covers the whole
  -- action.
  push $ CloseLogBlock key

{- | Open the block a whole Navigate action is filed under, and name it.

The block has to outlive the asks in the middle of a navigate -- the
destination, the skill -- so it is held in game state rather than by the
narrator, which is rebuilt every action. Everything a step narrates joins that
step's block, and step 4's skill test makes a block of its own inside step 4's.

The key is random because a block id must be unique for the life of the log:
the client collects a block by id across the whole of it, so a key reused next
round would fold two navigates into one.
-}
beginNavigateLog :: (ReverseQueue m, HasGameLogger m) => InvestigatorId -> m Text
beginNavigateLog iid = do
  token <- getRandom
  let key = "navigate:" <> tshow (token :: BatchId)
  who <- investigatorRefFor iid
  push $ OpenLogBlock key
  scenarioI18n
    $ scope "navigate"
    $ sendLog
    $ opensGroup key
    $ Log.action [ikeyScoped "header" ["investigator" ~> who]]
  pure key

-- | One step's block, inside the navigate's.
navigateStepBlock :: Text -> Int -> Text
navigateStepBlock key n = key <> "/step" <> tshow n

{- | Begin a step: open its block and announce it, as the investigator reaches
it.

A log entry cannot be queued -- only messages can -- so the rows ride the queue
as a 'ScenarioSpecific' that the scenario hands straight back to
'logNavigateStep'. Opened in step order, interleaved with the work each step
does, so the log reads in the order the action happened rather than announcing
all four up front.
-}
openNavigateStep :: ReverseQueue m => Text -> Int -> m ()
openNavigateStep key n = do
  push $ OpenLogBlock (navigateStepBlock key n)
  scenarioSpecific "navigate.step" (navigateStepBlock key n, n)

{- | End a step, so what the next one does is filed under the next one. Pushed
after the step's own work, which for steps 3 and 4 means inside the ask that
work hangs off.
-}
closeNavigateStep :: ReverseQueue m => Text -> Int -> m ()
closeNavigateStep key n = push $ CloseLogBlock (navigateStepBlock key n)

{- | The scenario's end of 'openNavigateStep'.

The banner that names the step is the block's own bar, and the step's printed
text is the first line inside it; everything the step then does lands under the
two.
-}
logNavigateStep :: HasGameLogger m => Value -> m ()
logNavigateStep val = do
  let (gid, n) = toResultDefault ("", 0 :: Int) val
  scenarioI18n
    $ scope "navigate"
    $ sendLogs
      [ opensGroup gid $ structure [ikeyScoped "stepLabel" ["step" ~> n]]
      , inGroupOf gid $ notice [ikeyScoped ("step" <> tshow n) (vars n)]
      ]
 where
  -- The icons go in as variables: an i18n variable may not be named after one,
  -- because the loader escapes those braces into literal text.
  vars 4 = ["combatIcon" ~> SkillCombat, "agilityIcon" ~> SkillAgility]
  vars _ = []

{- | The pair of abilities every River location prints: @{action}{action}@ Walk
Along the River as ability 1 and @{action}@ Navigate, once per round, as ability
2. A location using these answers 'UseThisAbility' 1 and 2 with
'resolveWalkAlongTheRiver' and 'resolveNavigate'.
-}
riverActions :: (Sourceable a, HasCardCode a, Be a LocationMatcher) => a -> [Ability]
riverActions a =
  [ restricted a 1 Here walkAlongTheRiverAction
  , playerLimit PerRound
      $ noAOO
      $ restricted a 2 (Here <> exists (navigateDestinations $ be a)) navigateAction
  ]

-- * The recurring chaos-token clause

{- | "At the end of the round, reveal a chaos token: if a [skull], [cultist],
[tablet], [elder_thing] or [auto_fail] symbol is revealed, ..." -- Mist in the
Valley, Unexpected Obstacles, Rapid, Something Is Stalking You and the Bear all
hang off the same five faces.
-}
symbolFaces :: [ChaosTokenFace]
symbolFaces = [Skull, Cultist, Tablet, ElderThing, AutoFail]

isSymbolFace :: ChaosToken -> Bool
isSymbolFace token = token.face `elem` symbolFaces

{- | "Civilized locations gain: '{action}: Resign.'" Agendas 2 and 3 both print
this, so the three Civilized locations carry it and read the agenda step rather
than the agendas reaching into the locations.
-}
civilizedResign :: (Sourceable a, HasCardCode a) => a -> Ability
civilizedResign a =
  restricted a 99 (Here <> notExists (AgendaWithStep 1))
    $ ActionAbility #resign Nothing (ActionCost 1)

{- | "Forced - If there are 2 clues on the Mountain Range (3 clues instead for a
3 or 4 player game): reveal it."

Both Uncharted locations whose unrevealed side is a Mountain Range print this,
and a location using it answers 'UseThisAbility' 98 by revealing itself.

@AnyWindow@ because the clue can land at any point -- the scenario reference's
[tablet] effect is the only thing that ever puts one there -- and 'onlyOnce'
because there is nothing left to do once it has flipped.
-}
mountainRangeReveal :: (Sourceable a, HasCardCode a) => a -> Ability
mountainRangeReveal a =
  onlyOnce $ restricted a 98 (CluesOnThis $ AtLeast $ ByPlayerCount 2 2 3 3) $ forced AnyWindow

-- * The Students' Fate deck

{- | "The lead investigator randomly takes a card from the Student's Fate deck
and reads the first part of it." Each card's first part reveals the location
that student was lost at; the second part waits until that location runs out of
clues.
-}
drawStudentsFate :: ReverseQueue m => m ()
drawStudentsFate = do
  lead <- getLead
  getScenarioDeck StudentsFateDeck >>= \case
    [] -> pure ()
    (card : _) -> do
      push $ RemoveCardFromScenarioDeck StudentsFateDeck card
      readStory lead card (toCardDef card)

-- * The students' fates

{- | "{reaction} If there are no more clues on this location, read the second part
of the <student>'s Fate story card."

Printed on Site of Ancient Stones, Swamp and Impenetrable Forest, one per
student. Each student has two versions and setup keeps only one, so the location
watches for either. The fate card is put aside by its own first part and stays on
the table, so the trigger is a condition and not a moment: it can be read at any
time once the clues are gone, including at a location that was already empty.
-}
fateReaction :: (Sourceable a, HasCardCode a, Be a LocationMatcher) => a -> [CardDef] -> Ability
fateReaction a fates =
  onlyOnce
    $ nonBlocking
    $ restricted a 1 (Here <> (thisExists a LocationWithoutClues <> mapOneOf (exists . asideFate) fates))
    $ freeReaction AnyWindow

{- | Resolving the aside card a second time is what reads its second part, with
the card itself back on screen the way reading one off the deck shows it.
-}
readSecondPartOfFate :: ReverseQueue m => InvestigatorId -> [CardDef] -> m ()
readSecondPartOfFate iid fates = for_ fates \fate -> selectEach (asideFate fate) (spotlightStory iid)

{- | Put a story card that is already on the table back on screen, and resolve it
once it is dismissed.

It cannot go back through @ReadStory@: that builds a fresh story from the card,
which for a fate card would hand it its first part all over again. This asks the
question @ReadStory@ would have -- the card in focus, one target label on it that
resolves the story -- which is what the client shows full screen.
-}
spotlightStory :: ReverseQueue m => InvestigatorId -> StoryId -> m ()
spotlightStory iid sid = do
  card <- field Story.StoryCard sid
  focusCards [card]
    $ lift
    $ chooseOneM iid
    $ targeting card.id
    $ push
    $ StoryMessage
    $ ResolveStory iid ResolveIt sid

{- | Read a story card that prints a choice: the card goes on screen and the
story asks its own question there, rather than behind @ReadStory@'s one-option
prompt. Those are the two messages that prompt would have run.
-}
readStoryAndChoose :: ReverseQueue m => InvestigatorId -> CardDef -> m ()
readStoryAndChoose iid def = do
  card <- fetchCard def
  let sid = StoryId (toCardCode card)
  {- The card the read uses IS the set-aside copy ('fetchCard' prefers one), and
  placing a story does not take it out of the pile -- nothing in the scenario
  runner removes a set-aside card for a story the way it does for a location. So
  it would sit there for the rest of the scenario, still offered to anything that
  looks in the pile. -}
  obtainCard card
  push $ StoryMessage $ PlaceStory card Unplaced
  push $ StoryMessage $ ResolveStory iid ResolveIt sid
  push $ MoveWithSkillTest $ StoryMessage $ ResolvedStory iid ResolveIt sid

-- | A fate card that has been put aside by its first part.
asideFate :: HasCardCode a => a -> StoryMatcher
asideFate fate = storyIs fate <> StoryWithPlacement Global

-- * Guides

guideAsset :: AssetMatcher
guideAsset = AssetWithTrait Guide

{- | The test the Guides' second reaction answers: "at stage 4 of a Navigate
action, or triggered by a River treachery card."

Stage 4 is the test the location's Navigate ability starts, so the action on that
ability is what names it; the other half is a River treachery's own test. Ambush
gains the River trait while you are on a River location, which is what that
clause is there for.
-}

{- | The skill test being resolved right now has not resolved yet.

The Guides' bonuses are offered at the @InitiatedSkillTest@ window, and that
window comes round again after the test has resolved -- at which point a bonus to
the skill value does nothing.

It has to be a criterion rather than part of the window matcher: the window
carries the skill test as it was when the window opened, so its result is always
@Unrun@ there, while 'DuringSkillTest' reads the live one. @ResultOneOf
[success, failure]@ is what distinguishes a resolved test, since a bare
@AnyResult@ matches @Unrun@ too.
-}
duringUnresolvedTest :: Criterion
duringUnresolvedTest =
  DuringSkillTest
    $ NotSkillTest
    $ SkillTestWithResult
    $ ResultOneOf [SuccessResult AnyValue, FailureResult AnyValue]

navigateOrRiverTest :: SkillTestMatcher
navigateOrRiverTest =
  SkillTestOneOf
    [ SkillTestSourceMatches $ SourceIsAbility $ AbilityIsAction Navigate
    , SkillTestSourceMatches $ SourceIsType TreacheryType <> SourceWithTrait River
    ]

{- | "At the beginning of the turn of an investigator in the same location, you
can give this card to this investigator, who takes control of it."

Shared by Sarcee Guide, Charlie Foxtail, Expedition Notebook and Norman Falkner.
-}
handOverGuideAbility :: (Sourceable a, HasCardCode a) => a -> Int -> Ability
handOverGuideAbility a n =
  restricted a n ControlsThis
    $ triggered (TurnBegins #after $ InvestigatorAt YourLocation <> NotYou) mempty
