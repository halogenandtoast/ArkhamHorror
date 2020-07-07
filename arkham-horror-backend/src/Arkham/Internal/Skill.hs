module Arkham.Internal.Skill where

import Arkham.Types.Skill
import Arkham.Types.Trait
import ClassyPrelude

data ArkhamSkillInternal = ArkhamSkillInternal
  { skillTraits :: HashSet ArkhamTrait
  , skillTestIcons :: [ArkhamSkillType]
  , skillActionCost :: Int
  }

skill :: [ArkhamSkillType] -> ArkhamSkillInternal
skill testIcons = ArkhamSkillInternal
  { skillTraits = mempty
  , skillTestIcons = testIcons
  , skillActionCost = 1
  }

deduction :: ArkhamSkillInternal
deduction = skill [ArkhamSkillIntellect]

guts :: ArkhamSkillInternal
guts = skill $ replicate 2 ArkhamSkillWillpower

overpower :: ArkhamSkillInternal
overpower = skill $ replicate 2 ArkhamSkillCombat

unexpectedCourage :: ArkhamSkillInternal
unexpectedCourage = skill $ replicate 2 ArkhamSkillWild

viciousBlow :: ArkhamSkillInternal
viciousBlow = skill [ArkhamSkillCombat]
