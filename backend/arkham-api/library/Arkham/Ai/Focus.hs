module Arkham.Ai.Focus (Focus (..), allFoci, focusForAction, focusForSkill, displayFocus) where

import Arkham.Action (Action (..))
import Arkham.Prelude
import Arkham.SkillType (SkillType (..))
import Control.Monad.Fail
import Data.Aeson.Types (Parser, toJSONKeyText)

{- | A coarse strategic role used by the AI to weigh investigators, cards,
abilities, and scenario objectives against one another.
-}
data Focus
  = CombatFocus
  | InvestigateFocus
  | EvadeFocus
  | SupportFocus
  | SurvivalFocus
  | MobilityFocus
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

allFoci :: [Focus]
allFoci = [minBound ..]

{- | The stable, lowercase wire key for a 'Focus'. Used for both JSON values and
JSON object keys (e.g. the @weights@ map) so the data file stays human-edited.
-}
focusKey :: Focus -> Text
focusKey = \case
  CombatFocus -> "combat"
  InvestigateFocus -> "investigate"
  EvadeFocus -> "evade"
  SupportFocus -> "support"
  SurvivalFocus -> "survival"
  MobilityFocus -> "mobility"

parseFocusText :: Text -> Parser Focus
parseFocusText = \case
  "combat" -> pure CombatFocus
  "investigate" -> pure InvestigateFocus
  "evade" -> pure EvadeFocus
  "support" -> pure SupportFocus
  "survival" -> pure SurvivalFocus
  "mobility" -> pure MobilityFocus
  other -> fail $ "unknown Focus: " <> unpack other

instance ToJSON Focus where
  toJSON = String . focusKey

instance FromJSON Focus where
  parseJSON = withText "Focus" parseFocusText

instance ToJSONKey Focus where
  toJSONKey = toJSONKeyText focusKey

instance FromJSONKey Focus where
  fromJSONKey = FromJSONKeyTextParser parseFocusText

-- | Human-readable label for surfacing a 'Focus' in the UI.
displayFocus :: Focus -> Text
displayFocus = \case
  CombatFocus -> "Combat"
  InvestigateFocus -> "Investigate"
  EvadeFocus -> "Evade"
  SupportFocus -> "Support"
  SurvivalFocus -> "Survival"
  MobilityFocus -> "Mobility"

{- | The 'Focus' an action contributes to, if any. Actions with no strategic
focus (e.g. 'Activate', 'Play', 'Resign', 'Explore', 'Circle') return
'Nothing' since their focus depends on the card being used.
-}
focusForAction :: Action -> Maybe Focus
focusForAction = \case
  Fight -> Just CombatFocus
  Engage -> Just CombatFocus
  Investigate -> Just InvestigateFocus
  Evade -> Just EvadeFocus
  Move -> Just MobilityFocus
  Resource -> Just SupportFocus
  Draw -> Just SupportFocus
  Parley -> Just SupportFocus
  _ -> Nothing

{- | The 'Focus' a skill icon maps to. Willpower has no dedicated test action, so
it maps to 'SurvivalFocus' (treats/saves).
-}
focusForSkill :: SkillType -> Focus
focusForSkill = \case
  SkillCombat -> CombatFocus
  SkillIntellect -> InvestigateFocus
  SkillAgility -> EvadeFocus
  SkillWillpower -> SurvivalFocus
