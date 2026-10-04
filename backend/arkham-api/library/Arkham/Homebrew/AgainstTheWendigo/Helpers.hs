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
import Arkham.Card.CardCode (HasCardCode)
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.GameEnv (getDistance)
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Homebrew.AgainstTheWendigo.Actions (pattern Navigate, pattern WalkAlongTheRiver)
import Arkham.Homebrew.AgainstTheWendigo.ScenarioDeckKeys (pattern StudentsFateDeck)
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Guide)
import Arkham.I18n
import Arkham.Distance (unDistance)
import Arkham.Id
import Arkham.Matcher
import Arkham.Card (toCardDef)
import Arkham.Helpers.Story (readStory)
import Arkham.Message (Message (CheckAttackOfOpportunity, RemoveCardFromScenarioDeck))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (forcedMoveTo)
import Arkham.Prelude
import Arkham.Source
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

{- | "Investigators in River locations cannot move or be moved onto another
River location, unless they perform Walk Along the River or Navigate."

Granted by the scenario to each investigator standing on the water, naming every
*other* river location. Both river actions move with 'forcedMoveTo', which is
not subject to it.
-}
riverMovementBan :: HasGame m => InvestigatorId -> m [ModifierType]
riverMovementBan iid =
  getMaybeLocation iid >>= \case
    Nothing -> pure []
    Just lid -> do
      onTheWater <- lid <=~> riverLocation
      if not onTheWater
        then pure []
        else map CannotEnter <$> select (riverLocation <> not_ (LocationWithId lid))

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
  -- The printed step 2 also says you cannot reveal a location during the
  -- action. Step 3 moves straight to the destination rather than walking the
  -- river one location at a time, so there is nothing in between to reveal.

  -- Steps 3 and 4: move, then test against the distance travelled.
  start <- getMaybeLocation iid
  destinations <- select $ riverLocation <> not_ (locationWithInvestigator iid)
  chooseOrRunOneM iid $ targets destinations \lid -> do
    mdistance <- maybe (pure Nothing) (`getDistance` lid) start
    let steps = maybe 1 (min 3 . max 1 . unDistance) mdistance
    forcedMoveTo source iid lid
    sid <- getRandom
    chooseOneM iid $ scenarioI18n $ scope "navigate" do
      labeled "testCombat" $ beginSkillTest sid iid source iid #combat (Fixed $ 1 + steps)
      labeled "testAgility" $ beginSkillTest sid iid source iid #agility (Fixed $ 1 + steps)

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
riverActions :: (Sourceable a, HasCardCode a) => a -> [Ability]
riverActions a =
  [ restricted a 1 Here walkAlongTheRiverAction
  , playerLimit PerRound $ restricted a 2 Here navigateAction
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

-- * Guides

guideAsset :: AssetMatcher
guideAsset = AssetWithTrait Guide

{- | "At the beginning of the turn of an investigator in the same location, you
can give this card to this investigator, who takes control of it."

Shared by Sarcee Guide, Charlie Foxtail, Expedition Notebook and Norman Falkner.
-}
handOverGuideAbility :: (Sourceable a, HasCardCode a) => a -> Int -> Ability
handOverGuideAbility a n =
  restricted a n ControlsThis
    $ triggered (TurnBegins #after $ InvestigatorAt YourLocation <> NotYou) mempty
