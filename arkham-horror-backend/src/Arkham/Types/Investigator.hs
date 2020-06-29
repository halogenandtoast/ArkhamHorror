module Arkham.Types.Investigator
  ( ArkhamInvestigator(..)
  )
where

import Arkham.Types.Card
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
  , aiCardCode :: ArkhamCardCode
  }
  deriving stock (Generic, Show)

instance Eq ArkhamInvestigator where
  a == b = aiCardCode a == aiCardCode b

instance ToJSON ArkhamInvestigator where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }

instance FromJSON ArkhamInvestigator where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
