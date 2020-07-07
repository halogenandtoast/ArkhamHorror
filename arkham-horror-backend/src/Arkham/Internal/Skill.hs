module Arkham.Internal.Skill
  ( ArkhamSkillInternal(..)
  , allSkills
  )
where

import Arkham.Types.Card
import Arkham.Types.Skill
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap

data ArkhamSkillInternal = ArkhamSkillInternal
  { skillTraits :: HashSet ArkhamTrait
  , skillTestIcons :: [ArkhamSkillType]
  , skillActionCost :: Int
  , skillCode :: ArkhamCardCode
  , skillName :: Text
  }

allSkills :: HashMap ArkhamCardCode ArkhamSkillInternal
allSkills = HashMap.fromList $ map
  (\s -> (skillCode s, s))
  [deduction, guts, overpower, unexpectedCourage, viciousBlow]

skill :: Text -> ArkhamCardCode -> [ArkhamSkillType] -> ArkhamSkillInternal
skill name code testIcons = ArkhamSkillInternal
  { skillTraits = mempty
  , skillTestIcons = testIcons
  , skillActionCost = 1
  , skillName = name
  , skillCode = code
  }

deduction :: ArkhamSkillInternal
deduction = skill "Deduction" "01039" [ArkhamSkillIntellect]

guts :: ArkhamSkillInternal
guts = skill "Guts" "01089" $ replicate 2 ArkhamSkillWillpower

overpower :: ArkhamSkillInternal
overpower = skill "Overpower" "01091" $ replicate 2 ArkhamSkillCombat

unexpectedCourage :: ArkhamSkillInternal
unexpectedCourage =
  skill "Unexpected Courage" "01093" $ replicate 2 ArkhamSkillWild

viciousBlow :: ArkhamSkillInternal
viciousBlow = skill "Vicious Blow" "01025" [ArkhamSkillCombat]
