module AH3e.Engine.Behavior where

import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Card (Encounter)
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State

data ComponentActionDef = ComponentActionDef
  { label :: Text
  , allowedWhileEngaged :: Bool
  , canPerform :: InvestigatorId -> GameM Bool
  , perform :: EffectCtx -> GameM ()
  }
  deriving stock Generic

data Reaction = Reaction {key :: Text, label :: Text, messages :: [Message]}
  deriving stock Generic

-- | What is about to be put down on the board, for a card that may stop it.
data Placement = PlacingDoom | PlacingMonster CardId
  deriving stock (Show, Eq)

data AssetBehavior = AssetBehavior
  { testDice :: CardId -> InvestigatorId -> TestState -> GameM (Maybe Int)
  , componentActions :: [ComponentActionDef]
  , stopsPlacement :: CardId -> InvestigatorId -> Placement -> SpaceId -> GameM [Reaction]
  {- ^ once per round, what this card offers when doom or a monster would be put
  down in its owner's neighborhood (Flux Stabilizer). A reaction's messages are
  run in place of the placement.
  -}
  , freeActions :: [ComponentActionDef]
  {- ^ what a card printed "during your turn, you may ..." offers: it costs none of
  their actions and stays available once both are spent.
  -}
  , reckoning :: Maybe Effect
  , reactions :: CardId -> Trigger -> GameM [Reaction]
  , moveAction :: Maybe (Int, Int)
  , tradeInNeighborhood :: Bool
  , freeRerollPerRound :: Bool
  -- ^ once per round, reroll one die while resolving a test at no cost
  , bonusDicePerRound :: CardId -> InvestigatorId -> GameM Int
  {- ^ once per round, this many additional dice join the pool while it is
  determined, before the roll (490.2d). Zero means the card has nothing to add
  and is not offered.
  -}
  , attackSkillInstead :: Maybe Skill
  -- ^ a skill its owner may test in place of strength as part of an attack action
  , evadeSkillInstead :: Maybe Skill
  -- ^ likewise in place of observation as part of an evade action
  , moveBySpell :: Maybe (Skill, Int, Int)
  {- ^ a spell taken instead of a normal move action: the skill to test, that
  test's modifier, and the spaces added to its result.
  -}
  , extraActions :: Int
  -- ^ additional actions its owner may perform during their turn (402.3)
  , afterGainedFromDeck :: Maybe Effect
  -- ^ resolved for its new owner after the card comes out of its deck, not after a trade
  , testOptions :: CardId -> InvestigatorId -> TestState -> GameM [Reaction]
  {- ^ what this card offers while a test of its owner's is resolving: a reroll or
  a die's result (490.3), successes, or anything else it prints "as part of" the
  action. Offered at the manipulate-dice step; whatever limit the card prints, its
  own messages record ('MarkAssetUsed' per round, 'MarkUsedInTest' per test).
  -}
  , undiscardable :: Bool
  -- ^ nothing discards this card, damage and horror included
  , poolDelta :: CardId -> InvestigatorId -> TestState -> GameM Int
  {- ^ dice this card adds to or takes from every pool its owner rolls, without
  being chosen and without taking hands (RATTLED's one fewer die).
  -}
  , dieBonus :: CardId -> InvestigatorId -> TestState -> GameM Int
  -- ^ added to the result of each die its owner rolls for this test
  , mayTakeEngagement :: Bool
  -- ^ its owner may step in when a monster would engage someone in their space
  , mayStopAttacks :: Bool
  -- ^ its owner may discard it to disengage and exhaust what is about to attack them
  , focusPerSkill :: Int
  {- ^ how many focus its owner may put on one skill, one unless a card says more
  (Overcome All Odds)
  -}
  , ignoredByMonsters :: CardId -> InvestigatorId -> GameM Bool
  {- ^ non-epic monsters pass its owner by while activating and do not engage
  them, until its owner attacks or damages the monster (Tattered Cloak). A card
  that only hides them somewhere in particular answers for itself.
  -}
  , ignoredWhileMoving :: Bool
  {- ^ monsters do not engage its owner while a move action is in flight, and go
  back to engaging them normally once it ends (Chuck Fergus).
  -}
  , handsDelta :: Int
  -- ^ hands' worth of assets its owner may use in a test beyond the usual two
  , afterMonsterDefeated :: CardId -> InvestigatorId -> CardId -> Source -> GameM [Message]
  {- ^ what this card does when a monster is defeated, whoever finished it and
  however. The monster is still on the board, so its traits can be read.
  -}
  , successOnFour :: Bool
  -- ^ while its owner tests, a four counts as a success as well (Dark Blessing)
  , bansConditions :: [ConditionName]
  {- ^ conditions its owner cannot hold: gaining one discards it instead, and
  holding this card discards the ones already held.
  -}
  , bansTraits :: [Trait]
  {- ^ likewise for whole traits, which is how WANTED keeps its holder out of the
  gangs' good books.
  -}
  , displayDelta :: CardId -> GameM Int
  -- ^ how many more cards the display holds while this card is in play
  , onDiscard :: CardId -> InvestigatorId -> GameM [Message]
  -- ^ what leaving play sets going, for a card that prints a cost for losing it
  , halfPricePerRound :: Bool
  {- ^ once per round, its owner may buy one card at half price, rounded up. The
  card says it does not stack, so it is not offered on a purchase already halved.
  -}
  , extraSuccesses :: CardId -> InvestigatorId -> TestState -> GameM Int
  {- ^ successes this card adds to the count beyond one per passing die, read as
  the test finishes (Shotgun's sixes).
  -}
  , preventsOwnHarm :: Maybe (HarmStat, Int, Int)
  {- ^ once per round, when this much or more of that harm is dealt to this card,
  prevent this much of it. Printed without a "may", so it is not offered, it just
  happens (Bulletproof Vest, Elder Sign Amulet).
  -}
  , insteadOfRemnant :: CardId -> InvestigatorId -> GameM [Reaction]
  {- ^ what this card takes in place of a remnant its owner would gain; offered
  alongside simply taking the remnant.
  -}
  , raiseInsteadOfReroll :: Bool
  {- ^ once per round, its owner may add one to a die instead of rerolling it,
  paying the reroll's cost either way (Research Notes).
  -}
  , afterGainClue :: CardId -> InvestigatorId -> GameM [Message]
  {- ^ what this card does when its owner gains clues. Not offered but done, for a
  card that says "you gain" rather than "you may" (Reporting Gig).
  -}
  , afterMonsterDamaged :: CardId -> InvestigatorId -> CardId -> Source -> GameM [Message]
  {- ^ what this card does after a monster takes damage, wherever the monster is
  and whoever dealt it. Consulted for every investigator in play, so a card can
  answer damage another investigator dealt (Lita Chantler).
  -}
  , afterHarm :: CardId -> InvestigatorId -> HarmPlan -> GameM [Message]
  {- ^ what this card does once a harm plan has landed, whether the harm reached
  the card or its owner. Still consulted for a card the harm destroyed.
  -}
  , preventsOneHarmPerRound :: Bool
  {- ^ once per round its owner may simply prevent one damage or one horror they
  would suffer, no test asked (Mama's Amulet)
  -}
  , damagePrevention :: CardId -> InvestigatorId -> HarmPlan -> GameM [Reaction]
  {- ^ once per round, offered to its owner while anyone would suffer damage,
  wherever they are (416.6). The reaction's messages leave what they prevent in
  'damagePrevented'.
  -}
  , buyOffers :: CardId -> InvestigatorId -> CardId -> Int -> GameM [Reaction]
  {- ^ what this card offers in place of paying a display card's price, asked once
  per card on sale with that card's price (Good Standing). The reaction buys, or
  does not; the display prompt comes back around either way.
  -}
  , tradesFocusAndTalents :: Bool
  {- ^ its holder's trades may also exchange focus tokens and talents (Mi-Go Brain
  Case), within both sides' focus limits.
  -}
  , replacesActivation :: CardId -> InvestigatorId -> CardId -> GameM [Reaction]
  {- ^ what its owner may do when that monster would activate, in place of the
  activation (Lure Monster). A reaction that lets the monster act anyway pushes
  'DoActivateMonster' itself.
  -}
  }
  deriving stock Generic

defaultAssetBehavior :: AssetBehavior
defaultAssetBehavior =
  AssetBehavior
    { testDice = \_ _ _ -> pure Nothing
    , componentActions = []
    , stopsPlacement = \_ _ _ _ -> pure []
    , freeActions = []
    , reckoning = Nothing
    , reactions = \_ _ -> pure []
    , moveAction = Nothing
    , tradeInNeighborhood = False
    , freeRerollPerRound = False
    , bonusDicePerRound = \_ _ -> pure 0
    , attackSkillInstead = Nothing
    , evadeSkillInstead = Nothing
    , moveBySpell = Nothing
    , extraActions = 0
    , afterGainedFromDeck = Nothing
    , preventsOwnHarm = Nothing
    , insteadOfRemnant = \_ _ -> pure []
    , raiseInsteadOfReroll = False
    , afterGainClue = \_ _ -> pure []
    , afterMonsterDamaged = \_ _ _ _ -> pure []
    , afterHarm = \_ _ _ -> pure []
    , testOptions = \_ _ _ -> pure []
    , undiscardable = False
    , poolDelta = \_ _ _ -> pure 0
    , dieBonus = \_ _ _ -> pure 0
    , mayTakeEngagement = False
    , mayStopAttacks = False
    , focusPerSkill = 1
    , ignoredByMonsters = \_ _ -> pure False
    , ignoredWhileMoving = False
    , handsDelta = 0
    , afterMonsterDefeated = \_ _ _ _ -> pure []
    , successOnFour = False
    , bansConditions = []
    , bansTraits = []
    , displayDelta = \_ -> pure 0
    , onDiscard = \_ _ -> pure []
    , halfPricePerRound = False
    , extraSuccesses = \_ _ _ -> pure 0
    , preventsOneHarmPerRound = False
    , damagePrevention = \_ _ _ -> pure []
    , buyOffers = \_ _ _ _ -> pure []
    , tradesFocusAndTalents = False
    , replacesActivation = \_ _ _ -> pure []
    }

{- | When a test asset adds dice: "+N skill as part of an X action", or "+N lore
while casting a spell".
-}

-- | Dice still in the pool, which every die option needs at least one of.
liveDiceCount :: TestState -> Int
liveDiceCount ts = length [d | d <- ts.dice, not d.removed]

data TestBonus = OnAction ActionKind Skill Int | WhileCasting Int

{- | A test asset whose bonuses add up for the tests they match; it is chosen like
any other test asset and takes its printed hands.
-}
testBonuses :: [TestBonus] -> AssetBehavior
testBonuses bonuses =
  defaultAssetBehavior
    & #testDice
    .~ \_ _ ts -> pure case [n | b <- bonuses, Just n <- [applies ts b]] of
      [] -> Nothing
      ns -> Just (sum ns)
 where
  applies ts = \case
    OnAction action skill n | ActionTest a _ <- ts.kind, a == action, ts.skill == skill -> Just n
    WhileCasting n | isJust ts.casting || isSpellTest ts.kind, ts.skill == Lore -> Just n
    _ -> Nothing

isSpellTest :: TestKind -> Bool
isSpellTest = \case
  SpellTest _ -> True
  _ -> False

{- | An "Action:" printed on a card: it spends an action like any other, is kept
back when it could accomplish nothing or its cost cannot be paid, and resolves
as the card's own effect.
-}
cardAction :: Text -> Effect -> AssetBehavior
cardAction lbl eff =
  defaultAssetBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = lbl
           , allowedWhileEngaged = False
           , canPerform = \iid -> do
               let (cost, body) = case eff of
                     Pay c rest -> (Just c, rest)
                     _ -> (Nothing, eff)
               affordable <- maybe (pure True) (canPayCost iid) cost
               useful <- effectUseful (EffectCtx iid (SourceInvestigator iid) Nothing) body
               pure (affordable && useful)
           , perform = \ctx -> push (ResolveEffect ctx eff)
           }
       ]

{- | An "Action:" printed on a spell: it spends an action, casts the spell -- which
pays its horror and may be interrupted -- and tests lore. The effect resolves
with the result in hand, so "equal to your test result" reads straight off it,
and a failed test resolves nothing.
-}
spellAction :: Text -> Int -> Effect -> AssetBehavior
spellAction lbl modifier eff =
  defaultAssetBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = lbl
           , allowedWhileEngaged = False
           , canPerform = \iid -> effectUseful (EffectCtx iid (SourceInvestigator iid) Nothing) eff
           , perform = \ctx -> case ctx.source of
               SourceCard cid -> push (castingTest ctx cid modifier (AfterEffect ctx eff NoEffect))
               _ -> pure ()
           }
       ]

{- | The cast plus its lore test, for a spell whose action is its own. Kept apart
from 'spellAction' for spells that must choose a target before they know their
modifier.
-}
castingTest :: EffectCtx -> CardId -> Int -> AfterTest -> Message
castingTest ctx cid modifier after =
  CastSpell
    ctx.investigator
    cid
    [BeginTest (newTest ctx.investigator Lore modifier (SpellTest cid) after) {casting = Just cid}]

-- | For a spell that prints "you can perform this action while engaged".
whileEngaged :: AssetBehavior -> AssetBehavior
whileEngaged = #componentActions . each . #allowedWhileEngaged .~ True

{- | What a monster card does beyond its printed stats. Epic monsters especially
carry text of their own, and it belongs with the monster rather than on whichever
codex card happens to describe it.
-}
data MonsterBehavior = MonsterBehavior
  { testOptions :: CardId -> InvestigatorId -> TestState -> GameM [Reaction]
  -- ^ what it offers whoever is testing against it, at the manipulate-dice step
  , healthDelta :: CardId -> GameM Int
  -- ^ added to its printed health as things stand, which a card may reduce
  , removedWhenDefeated :: Bool
  -- ^ goes back to the box rather than to the monster deck
  , afterAttack :: CardId -> InvestigatorId -> GameM [Message]
  -- ^ what it does to the investigator it has just attacked
  , afterDisengage :: CardId -> InvestigatorId -> GameM [Message]
  {- ^ what it does to whoever has just come away from it. Not offered but done,
  for a monster that prints it flatly rather than as a "may".
  -}
  }
  deriving stock Generic

defaultMonsterBehavior :: MonsterBehavior
defaultMonsterBehavior =
  MonsterBehavior
    { testOptions = \_ _ _ -> pure []
    , healthDelta = \_ -> pure 0
    , removedWhenDefeated = False
    , afterAttack = \_ _ -> pure []
    , afterDisengage = \_ _ -> pure []
    }

{- | A card that widens the reroll a focus paid for: the focus bought one die, and
this offers the rest. @howMany@ counts them from the test, zero meaning the pool.
-}
rerollInstead :: Text -> Text -> (TestState -> Int) -> AssetBehavior
rerollInstead key lbl howMany =
  defaultAssetBehavior
    & #reactions
    .~ \cid -> \case
      SpentFocusToReroll iid -> do
        mts <- use #test
        i <- getInvestigator iid
        used <- usedThisRound cid iid
        pure case mts of
          Just ts | not used -> do
            let n = if howMany ts == 0 then i.horror else howMany ts
            [ Reaction key lbl [MarkAssetUsed iid cid, RerollUpTo (SourceCard cid) (min (liveDiceCount ts) n)]
              | n > 0
              , liveDiceCount ts > 0
              ]
          _ -> []
      _ -> pure []

data CodexTrigger = CodexTrigger
  { key :: Text
  , once :: Bool
  , condition :: CodexEntry -> GameM Bool
  , action :: CodexEntry -> GameM ()
  }
  deriving stock Generic

data CodexBehavior = CodexBehavior
  { onAdd :: CodexEntry -> GameM ()
  , onFlip :: CodexEntry -> GameM ()
  , triggers :: [CodexTrigger]
  , reckoning :: CodexEntry -> Maybe Effect
  , afterMonsterSpawn :: CodexEntry -> CardId -> GameM [Message]
  -- ^ answers a monster arriving on the board, wherever it came from
  , afterMonsterDefeated :: CodexEntry -> CardId -> Source -> GameM [Message]
  -- ^ the monster is gone by now, so the source that finished it is carried
  , monsterHealthDelta :: CodexEntry -> CardId -> GameM Int
  -- ^ health this card adds to a monster it has marked or singled out
  , sheetDoomReplacement :: CodexEntry -> Int -> GameM (Maybe [Message])
  {- ^ what happens instead when doom would be placed on the scenario sheet; the
  first card to answer wins (Tsathoggua eats the city rather than the sheet)
  -}
  , componentActions :: [ComponentActionDef]
  , reactions :: CodexEntry -> InvestigatorId -> Trigger -> GameM [Reaction]
  -- ^ what a codex card offers an investigator when something triggers
  , afterAnomaly :: CodexEntry -> NeighborhoodId -> GameM [Message]
  -- ^ answers an anomaly opening in a neighborhood (406.3a)
  , encounterOverride :: CodexEntry -> InvestigatorId -> Encounter -> GameM (Maybe Effect)
  {- ^ replaces the encounter about to be read, for a card that answers what the
  text says rather than where it was drawn ("whenever your encounter text
  includes...").
  -}
  , blockedSpaces :: CodexEntry -> GameM [SpaceId]
  , spaceEncounter :: CodexEntry -> SpaceId -> Maybe Effect
  }
  deriving stock Generic

defaultCodexBehavior :: CodexBehavior
defaultCodexBehavior =
  CodexBehavior
    { onAdd = \_ -> pure ()
    , onFlip = \_ -> pure ()
    , triggers = []
    , reckoning = const Nothing
    , afterMonsterSpawn = \_ _ -> pure []
    , afterMonsterDefeated = \_ _ _ -> pure []
    , monsterHealthDelta = \_ _ -> pure 0
    , sheetDoomReplacement = \_ _ -> pure Nothing
    , componentActions = []
    , reactions = \_ _ _ -> pure []
    , afterAnomaly = \_ _ -> pure []
    , encounterOverride = \_ _ _ -> pure Nothing
    , blockedSpaces = \_ -> pure []
    , spaceEncounter = \_ _ -> Nothing
    }

data InvestigatorBehavior = InvestigatorBehavior
  { componentActions :: [ComponentActionDef]
  , reactions :: InvestigatorId -> Trigger -> GameM [Reaction]
  , testOptions :: InvestigatorId -> TestState -> GameM [Reaction]
  -- ^ what the sheet itself offers while its investigator's test resolves
  , mayTakeEngagement :: Bool
  -- ^ this investigator may step in when a monster would engage someone beside them
  , successOnSix :: Bool
  -- ^ only a six counts for this investigator, whatever else is in play (Rex Murphy)
  , bansConditions :: [ConditionName]
  -- ^ conditions this investigator cannot hold at all
  , bansTraits :: [Trait]
  -- ^ likewise for whole traits
  , castWithDamage :: Bool
  -- ^ may suffer damage instead of horror while casting a spell
  , paidCastLoreBonus :: Int
  -- ^ +lore while casting a spell paid for with damage or remnants
  , damagePrevention :: InvestigatorId -> HarmPlan -> GameM [Reaction]
  {- ^ what the sheet itself offers while anyone would suffer harm (416.6), the
  same way a card may. The reaction's messages leave what they prevent in
  'damagePrevented' and 'horrorPrevented'.
  -}
  }
  deriving stock Generic

defaultInvestigatorBehavior :: InvestigatorBehavior
defaultInvestigatorBehavior =
  InvestigatorBehavior
    { componentActions = []
    , reactions = \_ _ -> pure []
    , testOptions = \_ _ -> pure []
    , mayTakeEngagement = False
    , successOnSix = False
    , bansConditions = []
    , bansTraits = []
    , castWithDamage = False
    , paidCastLoreBonus = 0
    , damagePrevention = \_ _ -> pure []
    }

data Behaviors = Behaviors
  { assets :: Map CardCode AssetBehavior
  , monsters :: Map CardCode MonsterBehavior
  , codex :: Map ArchiveNumber CodexBehavior
  , investigators :: Map InvestigatorId InvestigatorBehavior
  , customAfterTests :: Map Text (Source -> Int -> GameM ())
  {- ^ what a card does with its own test's result, for a test whose ending is the
  card's business alone ('AfterCustom').
  -}
  , customEffects :: Map Text (EffectCtx -> GameM ())
  , customActivations :: Map Text (CardId -> GameM ())
  }
  deriving stock Generic

instance Semigroup Behaviors where
  a <> b =
    Behaviors
      (a.assets <> b.assets)
      (a.monsters <> b.monsters)
      (a.codex <> b.codex)
      (a.investigators <> b.investigators)
      (a.customAfterTests <> b.customAfterTests)
      (a.customEffects <> b.customEffects)
      (a.customActivations <> b.customActivations)

instance Monoid Behaviors where
  mempty = Behaviors mempty mempty mempty mempty mempty mempty mempty
