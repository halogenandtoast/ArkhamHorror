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
  deriving stock (Generic, Show)

instance ToJSON ArkhamInvestigator where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }

instance FromJSON ArkhamInvestigator where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
