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
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Distance (unDistance)
import Arkham.GameEnv (getDistance)
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
import Arkham.Location.Types (LocationAttrs)
import Arkham.Matcher
import Arkham.Message (
  Message (CheckAttackOfOpportunity, MoveWithSkillTest, RemoveCardFromScenarioDeck, StoryMessage),
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
import Arkham.Source
import Arkham.Story.Types qualified as Story
import Arkham.Trait (Trait (River))

-- * Text

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "againstTheWendigo" a

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "scenario" a

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
resolveNavigate :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
resolveNavigate (toSource -> source) iid = do
  -- Step 1: every ready enemy engaged with you attacks.
  push $ CheckAttackOfOpportunity iid False Nothing

  -- Step 2: everything disengages, and nothing may re-engage during the action.
  engaged <- select $ enemyEngagedWith iid <> ReadyEnemy
  for_ engaged disengageFromAll
  roundModifier source iid CannotBeEngaged
  -- Step 2 also forbids revealing a location during the action. Step 3 moves
  -- straight to the destination, so nothing in between can be revealed, and
  -- 'navigateDestinations' keeps the destination itself off the table unless it
  -- is already charted.

  -- Steps 3 and 4: move, then test against the distance travelled.
  start <- getMaybeLocation iid
  destinations <- select $ navigateDestinations (locationWithInvestigator iid)
  chooseOrRunOneM iid $ targets destinations \lid -> do
    mdistance <- maybe (pure Nothing) (`getDistance` lid) start
    let steps = maybe 1 (min 3 . max 1 . unDistance) mdistance
    forcedMoveTo source iid lid
    sid <- getRandom
    chooseOneM iid $ skillsLabeled [#combat, #agility] \sk ->
      beginSkillTest sid iid source iid sk (Fixed $ 1 + steps)

{- | The damage step 4 deals on a failure: "take 1 damage, or 2 damage if you
fail by 3 or more". The amount failed by rides on the 'FailedSkillTest' message.
-}
navigateFailureDamage :: Int -> Int
navigateFailureDamage failedBy = if failedBy >= 3 then 2 else 1

{- | The pair of abilities every River location prints: @{action}{action}@ Walk
Along the River as ability 1 and @{action}@ Navigate, once per round, as ability
2. A location using these answers 'UseThisAbility' 1 and 2 with
'resolveWalkAlongTheRiver' and 'resolveNavigate'.
-}
riverActions :: (Sourceable a, HasCardCode a, Be a LocationMatcher) => a -> [Ability]
riverActions a =
  [ restricted a 1 Here walkAlongTheRiverAction
  , playerLimit PerRound
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
  push $ StoryMessage $ PlaceStory card Unplaced
  push $ StoryMessage $ ResolveStory iid ResolveIt sid
  push $ MoveWithSkillTest $ StoryMessage $ ResolvedStory ResolveIt sid

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
