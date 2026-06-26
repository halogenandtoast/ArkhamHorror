{-# LANGUAGE TemplateHaskell #-}

module Arkham.Ai.Tags (
  AiTags (..),
  InvestigatorTag (..),
  CardTag (..),
  AbilityTag (..),
  ScenarioTag (..),
  ActObjective (..),
  Objective (..),
  GameValueSpec (..),
  AgendaInfo (..),
  aiTags,
  lookupInvestigatorTag,
  lookupCardTag,
  lookupAbilityTag,
  lookupScenarioTag,
  lookupActObjective,
) where

import Arkham.Action (Action)
import Arkham.Ai.Focus (Focus)
import Arkham.Card.CardCode (CardCode (..))
import Arkham.Prelude
import Control.Monad.Fail
import Data.FileEmbed (embedFile)
import Data.Map.Strict qualified as Map

{- | Static, hand-authored strategic metadata that augments the engine's card
data for the AI investigator. Decoded once from @data/ai-tags.json@.

The JSON keys for every @CardCode@-keyed map are BARE card codes (e.g.
@"01020"@), not the @c@-prefixed wire form the engine uses elsewhere; we
therefore decode those maps via 'Text' keys and wrap them with 'CardCode'
rather than relying on 'CardCode'\'s own JSON key instances.
-}
data AiTags = AiTags
  { aiInvestigators :: Map CardCode InvestigatorTag
  , aiCards :: Map CardCode CardTag
  , aiScenarios :: Map CardCode ScenarioTag
  }
  deriving stock (Show, Eq, Generic)

data InvestigatorTag = InvestigatorTag
  { itName :: Text
  , itDeckFocus :: Focus
  , itWeights :: Map Focus Int
  , itAbilities :: Map Int AbilityTag
  , itNotes :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

data CardTag = CardTag
  { ctFocuses :: [Focus]
  , ctRole :: Maybe Text
  , ctWeight :: Int
  , ctAbilities :: Map Int AbilityTag
  , ctNotes :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

data AbilityTag = AbilityTag
  { abFocuses :: [Focus]
  , abAction :: Maybe Action
  , abNotes :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

data ScenarioTag = ScenarioTag
  { stName :: Text
  , stActs :: Map CardCode ActObjective
  , stAgendas :: Map CardCode AgendaInfo
  }
  deriving stock (Show, Eq, Generic)

data ActObjective = ActObjective
  { aoFocus :: Focus
  , aoObjective :: Objective
  }
  deriving stock (Show, Eq, Generic)

data Objective
  = SpendCluesObjective {ocAmount :: GameValueSpec, ocScope :: Text}
  | DefeatEnemyObjective {deTarget :: CardCode}
  | OtherObjective {otNote :: Text}
  deriving stock (Show, Eq, Generic)

data GameValueSpec = StaticV Int | PerPlayerV Int
  deriving stock (Show, Eq, Generic)

data AgendaInfo = AgendaInfo
  { agThreshold :: GameValueSpec
  , agNote :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- Re-key helpers: the data file uses bare card codes, so we decode the maps
-- with 'Text' keys and convert, sidestepping 'CardCode'\'s @c@-prefixing key
-- instances.
cardCodeMap :: Map Text v -> Map CardCode v
cardCodeMap = Map.mapKeys CardCode

textKeyMap :: Map CardCode v -> Map Text v
textKeyMap = Map.mapKeys unCardCode

instance FromJSON AiTags where
  parseJSON = withObject "AiTags" \o -> do
    investigators <- o .:? "investigators" .!= mempty
    cards <- o .:? "cards" .!= mempty
    scenarios <- o .:? "scenarios" .!= mempty
    pure
      AiTags
        { aiInvestigators = cardCodeMap investigators
        , aiCards = cardCodeMap cards
        , aiScenarios = cardCodeMap scenarios
        }

instance ToJSON AiTags where
  toJSON t =
    object
      [ "investigators" .= textKeyMap (aiInvestigators t)
      , "cards" .= textKeyMap (aiCards t)
      , "scenarios" .= textKeyMap (aiScenarios t)
      ]

instance FromJSON InvestigatorTag where
  parseJSON = withObject "InvestigatorTag" \o -> do
    itName <- o .: "name"
    itDeckFocus <- o .: "deckFocus"
    itWeights <- o .:? "weights" .!= mempty
    itAbilities <- o .:? "abilities" .!= mempty
    itNotes <- o .:? "notes"
    pure InvestigatorTag {..}

instance ToJSON InvestigatorTag where
  toJSON t =
    object
      [ "name" .= itName t
      , "deckFocus" .= itDeckFocus t
      , "weights" .= itWeights t
      , "abilities" .= itAbilities t
      , "notes" .= itNotes t
      ]

instance FromJSON CardTag where
  parseJSON = withObject "CardTag" \o -> do
    ctFocuses <- o .:? "focuses" .!= []
    ctRole <- o .:? "role"
    ctWeight <- o .:? "weight" .!= 1
    ctAbilities <- o .:? "abilities" .!= mempty
    ctNotes <- o .:? "notes"
    pure CardTag {..}

instance ToJSON CardTag where
  toJSON t =
    object
      [ "focuses" .= ctFocuses t
      , "role" .= ctRole t
      , "weight" .= ctWeight t
      , "abilities" .= ctAbilities t
      , "notes" .= ctNotes t
      ]

instance FromJSON AbilityTag where
  parseJSON = withObject "AbilityTag" \o -> do
    abFocuses <- o .:? "focuses" .!= []
    abAction <- o .:? "action"
    abNotes <- o .:? "notes"
    pure AbilityTag {..}

instance ToJSON AbilityTag where
  toJSON t =
    object
      [ "focuses" .= abFocuses t
      , "action" .= abAction t
      , "notes" .= abNotes t
      ]

instance FromJSON ScenarioTag where
  parseJSON = withObject "ScenarioTag" \o -> do
    name <- o .: "name"
    acts <- o .:? "acts" .!= mempty
    agendas <- o .:? "agendas" .!= mempty
    pure
      ScenarioTag
        { stName = name
        , stActs = cardCodeMap acts
        , stAgendas = cardCodeMap agendas
        }

instance ToJSON ScenarioTag where
  toJSON t =
    object
      [ "name" .= stName t
      , "acts" .= textKeyMap (stActs t)
      , "agendas" .= textKeyMap (stAgendas t)
      ]

instance FromJSON ActObjective where
  parseJSON = withObject "ActObjective" \o ->
    ActObjective <$> o .: "focus" <*> o .: "objective"

instance ToJSON ActObjective where
  toJSON t = object ["focus" .= aoFocus t, "objective" .= aoObjective t]

instance FromJSON Objective where
  parseJSON = withObject "Objective" \o -> do
    kind <- o .: "kind"
    case (kind :: Text) of
      "spendClues" -> SpendCluesObjective <$> o .: "amount" <*> o .:? "scope" .!= "anywhere"
      "defeatEnemy" -> DefeatEnemyObjective . CardCode <$> o .: "target"
      "other" -> OtherObjective <$> o .: "note"
      other -> fail $ "unknown objective kind: " <> unpack other

instance ToJSON Objective where
  toJSON = \case
    SpendCluesObjective amount scope ->
      object ["kind" .= ("spendClues" :: Text), "amount" .= amount, "scope" .= scope]
    DefeatEnemyObjective target ->
      object ["kind" .= ("defeatEnemy" :: Text), "target" .= unCardCode target]
    OtherObjective note ->
      object ["kind" .= ("other" :: Text), "note" .= note]

instance FromJSON GameValueSpec where
  parseJSON = withObject "GameValueSpec" \o -> do
    mStatic <- o .:? "static"
    mPer <- o .:? "perPlayer"
    case (mStatic, mPer) of
      (Just n, _) -> pure (StaticV n)
      (Nothing, Just n) -> pure (PerPlayerV n)
      (Nothing, Nothing) -> fail "GameValueSpec requires 'static' or 'perPlayer'"

instance ToJSON GameValueSpec where
  toJSON = \case
    StaticV n -> object ["static" .= n]
    PerPlayerV n -> object ["perPlayer" .= n]

instance FromJSON AgendaInfo where
  parseJSON = withObject "AgendaInfo" \o ->
    AgendaInfo <$> o .: "threshold" <*> o .:? "note"

instance ToJSON AgendaInfo where
  toJSON t = object ["threshold" .= agThreshold t, "note" .= agNote t]

{- | The decoded tag table, embedded at compile time from
@data/ai-tags.json@ (resolved relative to the @backend/arkham-api@ package
directory). A malformed file fails the build loudly.
-}
aiTags :: AiTags
aiTags =
  either (error . ("ai-tags.json failed to parse: " <>)) id
    $ eitherDecodeStrict $(embedFile "data/ai-tags.json")

lookupInvestigatorTag :: CardCode -> Maybe InvestigatorTag
lookupInvestigatorTag code = Map.lookup code (aiInvestigators aiTags)

lookupCardTag :: CardCode -> Maybe CardTag
lookupCardTag code = Map.lookup code (aiCards aiTags)

{- | Look up an ability tag by @(card code, ability index)@. Card-level tags win
over investigator-level tags when both define the index.
-}
lookupAbilityTag :: CardCode -> Int -> Maybe AbilityTag
lookupAbilityTag code idx =
  (lookupCardTag code >>= Map.lookup idx . ctAbilities)
    <|> (lookupInvestigatorTag code >>= Map.lookup idx . itAbilities)

lookupScenarioTag :: CardCode -> Maybe ScenarioTag
lookupScenarioTag code = Map.lookup code (aiScenarios aiTags)

lookupActObjective :: CardCode -> CardCode -> Maybe ActObjective
lookupActObjective scenarioCode actCode =
  lookupScenarioTag scenarioCode >>= Map.lookup actCode . stActs
