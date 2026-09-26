module AH3e.Engine.Behavior where

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
  , extraActions :: Int
  -- ^ additional actions its owner may perform during their turn (402.3)
  , afterGainedFromDeck :: Maybe Effect
  -- ^ resolved for its new owner after the card comes out of its deck, not after a trade
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
    , extraActions = 0
    , afterGainedFromDeck = Nothing
    , damagePrevention = \_ _ _ -> pure []
    }

{- | When a test asset adds dice: "+N skill as part of an X action", or "+N lore
while casting a spell".
-}
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

-- | One die per clue you hold, plus one per clue in your neighborhood.
aliceLuxleyDice :: CardId -> InvestigatorId -> GameM Int
aliceLuxleyDice _ iid = do
  i <- getInvestigator iid
  here <- investigatorNeighborhood iid >>= maybe (pure 0) (fmap (.clues) . getNeighborhood)
  pure (i.clues + here)

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
      (a.customEffects <> b.customEffects)
      (a.customActivations <> b.customActivations)

instance Monoid Behaviors where
  mempty = Behaviors mempty mempty mempty mempty mempty
