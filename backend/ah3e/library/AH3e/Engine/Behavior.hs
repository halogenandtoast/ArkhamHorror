module AH3e.Engine.Behavior where

import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
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

data AssetBehavior = AssetBehavior
  { testDice :: CardId -> InvestigatorId -> TestState -> GameM (Maybe Int)
  , componentActions :: [ComponentActionDef]
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
  , ignoredByMonsters :: Bool
  {- ^ non-epic monsters pass its owner by while activating and do not engage
  them, until its owner attacks or damages the monster (Tattered Cloak).
  -}
  , successOnFour :: Bool
  -- ^ while its owner tests, a four counts as a success as well (Dark Blessing)
  , bansConditions :: [ConditionName]
  {- ^ conditions its owner cannot hold: gaining one discards it instead, and
  holding this card discards the ones already held.
  -}
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
  , damagePrevention :: CardId -> InvestigatorId -> HarmPlan -> GameM [Reaction]
  {- ^ once per round, offered to its owner while anyone would suffer damage,
  wherever they are (416.6). The reaction's messages leave what they prevent in
  'damagePrevented'.
  -}
  }
  deriving stock Generic

defaultAssetBehavior :: AssetBehavior
defaultAssetBehavior =
  AssetBehavior
    { testDice = \_ _ _ -> pure Nothing
    , componentActions = []
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
    , afterGainClue = \_ _ -> pure []
    , afterMonsterDamaged = \_ _ _ _ -> pure []
    , afterHarm = \_ _ _ -> pure []
    , testOptions = \_ _ _ -> pure []
    , ignoredByMonsters = False
    , successOnFour = False
    , bansConditions = []
    , halfPricePerRound = False
    , extraSuccesses = \_ _ _ -> pure 0
    , damagePrevention = \_ _ _ -> pure []
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
  , componentActions :: [ComponentActionDef]
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
    , componentActions = []
    , blockedSpaces = \_ -> pure []
    , spaceEncounter = \_ _ -> Nothing
    }

data InvestigatorBehavior = InvestigatorBehavior
  { componentActions :: [ComponentActionDef]
  , reactions :: InvestigatorId -> Trigger -> GameM [Reaction]
  , castWithDamage :: Bool
  -- ^ may suffer damage instead of horror while casting a spell
  , paidCastLoreBonus :: Int
  -- ^ +lore while casting a spell paid for with damage or remnants
  }
  deriving stock Generic

defaultInvestigatorBehavior :: InvestigatorBehavior
defaultInvestigatorBehavior =
  InvestigatorBehavior
    { componentActions = []
    , reactions = \_ _ -> pure []
    , castWithDamage = False
    , paidCastLoreBonus = 0
    }

data Behaviors = Behaviors
  { assets :: Map CardCode AssetBehavior
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
      (a.codex <> b.codex)
      (a.investigators <> b.investigators)
      (a.customAfterTests <> b.customAfterTests)
      (a.customEffects <> b.customEffects)
      (a.customActivations <> b.customActivations)

instance Monoid Behaviors where
  mempty = Behaviors mempty mempty mempty mempty mempty mempty
