module Arkham.Types.Investigator where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

newtype ArkhamSkillWillpower = ArkhamSkillWillpower { unArkhamSkillWillpower :: Int }
  deriving newtype (ToJSON)

newtype ArkhamSkillIntellect = ArkhamSkillIntellect { unArkhamSkillIntellect :: Int }
  deriving newtype (ToJSON)

newtype ArkhamSkillCombat = ArkhamSkillCombat { unArkhamSkillCombat :: Int }
  deriving newtype (ToJSON)

newtype ArkhamSkillAgility = ArkhamSkillAgility { unArkhamSkillAgility :: Int }
  deriving newtype (ToJSON)

data ArkhamInvestigator = ArkhamInvestigator
  { aiName :: Text
  , aiImage :: Text
  , aiPortrait :: Text
  , aiWillpower :: ArkhamSkillWillpower
  , aiIntellect :: ArkhamSkillIntellect
  , aiCombat :: ArkhamSkillCombat
  , aiAgility :: ArkhamSkillAgility
  }
  deriving stock (Generic)

instance ToJSON ArkhamInvestigator where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
