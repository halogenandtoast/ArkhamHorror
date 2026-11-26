module Arkham.ModifierData (
  module Arkham.ModifierData,
) where

import Arkham.Prelude

import Arkham.Campaigns.TheScarletKeys.Key.Id
import Arkham.Card.CardCode (CardCode)
import Arkham.ChaosBag.RevealStrategy (RevealStrategy)
import Arkham.ChaosToken.Types (ChaosTokenFace)
import Arkham.Id
import Arkham.Json
import Arkham.Modifier
import Arkham.SkillType

newtype ModifierData = ModifierData {mdModifiers :: [Modifier]}
  deriving stock (Show, Eq, Generic)

instance ToJSON ModifierData where
  toJSON = genericToJSON $ aesonOptions $ Just "md"
  toEncoding = genericToEncoding $ aesonOptions $ Just "md"

data LocationMetadata = LocationMetadata
  { lmConnectedLocations :: [LocationId]
  , -- The subset of the above a modifier granted rather than the card printing, which
    -- the map draws from the single location instead of from its group's box.
    lmGrantedConnections :: [LocationId]
  , lmInvestigators :: [InvestigatorId]
  , lmEnemies :: [EnemyId]
  , lmTreacheries :: [TreacheryId]
  , lmAssets :: [AssetId]
  , lmEvents :: [EventId]
  , lmScarletKeys :: [ScarletKeyId]
  , -- Shroud as the engine will use it, and as the card prints it. Same reason
    -- as the enemy stats: the client must never apply a shroud modifier itself.
    -- Both are Nothing together -- an unrevealed location, or one with no
    -- shroud at all, has nothing to report.
    lmModifiedShroud :: Maybe Int
  , lmPrintedShroud :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON LocationMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "lm"
  toEncoding = genericToEncoding $ aesonOptions $ Just "lm"

newtype ConnectionData = ConnectionData {cdConnectedLocations :: [LocationId]}
  deriving stock (Show, Eq, Generic)

instance ToJSON ConnectionData where
  toJSON = genericToJSON $ aesonOptions $ Just "cd"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cd"

data EnemyMetadata = EnemyMetadata
  { emEngagedInvestigators :: [InvestigatorId]
  , emTreacheries :: [TreacheryId]
  , emAssets :: [AssetId]
  , emEvents :: [EventId]
  , emSkills :: [SkillId]
  , emModifiers :: [Modifier]
  , emScarletKeys :: [ScarletKeyId]
  , emStories :: [StoryId]
  , -- The enemy's stats as the engine will actually use them, and as the card
    -- prints them. The client cannot work either out for itself: an enemy's
    -- `fight`/`health`/`evade` attrs are a @GameCalculation@, whose number the
    -- frontend decoder discards, and applying the modifiers by hand there
    -- would re-derive the min clamps and `AsIf*` overrides this field already
    -- handles. Sent so the UI can say "1 -> 3" rather than only "+2".
    --
    -- Named apart from the attrs' own `fight`/`health`/`evade`: `With` unions
    -- the metadata OVER the entity, so a shared key would replace them.
    emModifiedFight :: Maybe Int
  , emModifiedHealth :: Maybe Int
  , emModifiedEvade :: Maybe Int
  , emPrintedFight :: Maybe Int
  , emPrintedHealth :: Maybe Int
  , emPrintedEvade :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON EnemyMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "em"
  toEncoding = genericToEncoding $ aesonOptions $ Just "em"

data StoryMetadata = StoryMetadata
  { smModifiers :: [Modifier]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON StoryMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "sm"
  toEncoding = genericToEncoding $ aesonOptions $ Just "sm"

data TreacheryMetadata = TreacheryMetadata
  { tmPeril :: Bool
  , tmTreacheries :: [TreacheryId]
  {- ^ Treacheries attached to THIS treachery, so the client can draw them on
    it. Acts, agendas, assets and enemies all carry the same list; without it a
    card attached to a treachery (Ages Unwound's /Unleashed Chaos (Mutation)/,
    which attaches to whatever the draw turned up) is in play and invisible.
  -}
  , tmModifiers :: [Modifier]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TreacheryMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "tm"
  toEncoding = genericToEncoding $ aesonOptions $ Just "tm"

data AssetMetadata = AssetMetadata
  { amEvents :: [EventId]
  , amAssets :: [AssetId]
  , amTreacheries :: [TreacheryId]
  , amEnemies :: [EnemyId]
  , amModifiers :: [Modifier]
  , amPermanent :: Bool
  , amScarletKeys :: [ScarletKeyId]
  , -- TOTAL health and sanity after modifiers, and as printed -- what the
    -- asset can take, not what it has left. Both halves of a pair are present
    -- or absent together, and a spirit reports neither.
    -- See 'assetModifiedHealth' / 'assetModifiedSanity'.
    amModifiedHealth :: Maybe Int
  , amModifiedSanity :: Maybe Int
  , amPrintedHealth :: Maybe Int
  , amPrintedSanity :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AssetMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "am"
  toEncoding = genericToEncoding $ aesonOptions $ Just "am"

{- | One card's effect on one chaos token face, as the skill test window shows it.

Gathered by 'getSkillTestValueBreakdown' from two places that mean the same
thing to a player: an 'AddChaosTokenValue' modifier already in play
(@ctfeApplied = True@ -- its value is part of 'ctveValue' because the engine
really will add it), and an ability that declared the effect through
'abilityChaosTokenEffects' but has not resolved yet (@ctfeApplied = False@).

A card is listed once per face: a declaration is dropped when the same card
already has the matching modifier applied.
-}
data ChaosTokenFaceEffect = ChaosTokenFaceEffect
  { ctfeName :: Maybe Text
  -- ^ the card's name, resolved here so the client needs no card lookup
  , ctfeCardCode :: Maybe CardCode
  , ctfeValue :: Maybe Int
  -- ^ the value this adds to the test, when it has a numeric part
  , ctfeText :: Maybe Text
  -- ^ the prose half, from the declaring ability's tooltip; may be an i18n key
  , ctfeApplied :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ChaosTokenFaceEffect where
  toJSON = genericToJSON $ aesonOptions $ Just "ctfe"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ctfe"

data ChaosTokenValueEntry = ChaosTokenValueEntry
  { ctveFace :: ChaosTokenFace
  , ctveCount :: Int
  , ctveValue :: Maybe Int
  , ctveAutoFail :: Bool
  , ctveAutoSuccess :: Bool
  , ctveRevealsAnother :: Bool
  , ctveEffects :: [ChaosTokenFaceEffect]
  -- ^ who else is acting on this face; see 'ChaosTokenFaceEffect'
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ChaosTokenValueEntry where
  toJSON = genericToJSON $ aesonOptions $ Just "ctve"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ctve"

data SkillTestValueBreakdown = SkillTestValueBreakdown
  { stvbTokens :: [ChaosTokenValueEntry]
  , stvbSkillValue :: Int
  , stvbDifficulty :: Int
  , stvbFailTies :: Bool
  , stvbAutoFailIfSucceedByAtLeast :: [Int]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SkillTestValueBreakdown where
  toJSON = genericToJSON $ aesonOptions $ Just "stvb"
  toEncoding = genericToEncoding $ aesonOptions $ Just "stvb"

data SkillTestMetadata = SkillTestMetadata
  { stmModifiedSkillValue :: Int
  , stmModifiedDifficulty :: Int
  , stmSkills :: [SkillType]
  , stmModifiers :: [Modifier]
  , stmValueBreakdown :: Maybe SkillTestValueBreakdown
  , -- The reveal strategy as it stands; see 'getSkillTestRevealStrategy'.
    stmRevealStrategy :: RevealStrategy
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SkillTestMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "stm"
  toEncoding = genericToEncoding $ aesonOptions $ Just "stm"

data ActMetadata = ActMetadata
  { actmTreacheries :: [TreacheryId]
  , actmModifiers :: [Modifier]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ActMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "actm"
  toEncoding = genericToEncoding $ aesonOptions $ Just "actm"

data AgendaMetadata = AgendaMetadata
  { agendamTreacheries :: [TreacheryId]
  , agendamModifiers :: [Modifier]
  , -- The doom the agenda advances at, as printed (resolved for the player
    -- count) and after any 'DoomThresholdModifier'. Nothing for an agenda
    -- with no threshold. See 'agendaDoomThresholds'.
    agendamPrintedDoomThreshold :: Maybe Int
  , agendamModifiedDoomThreshold :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AgendaMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "agendam"
  toEncoding = genericToEncoding $ aesonOptions $ Just "agendam"
