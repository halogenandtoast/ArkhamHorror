module Arkham.Types.Investigator where

import Arkham.Types.Skill
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data ArkhamInvestigator = ArkhamInvestigator
  { aiName :: Text
  , aiImage :: Text
  , aiPortrait :: Text
  , aiWillpower :: ArkhamSkill 'ArkhamSkillWillpower
  , aiIntellect :: ArkhamSkill 'ArkhamSkillIntellect
  , aiCombat :: ArkhamSkill 'ArkhamSkillCombat
  , aiAgility :: ArkhamSkill 'ArkhamSkillAgility
  }
  deriving stock (Generic)

instance ToJSON ArkhamInvestigator where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
