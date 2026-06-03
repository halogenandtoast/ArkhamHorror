module Arkham.Skill.CardDefs.ReturnTo where

import Arkham.Skill.CardDefs.Import

riseToTheOccasion3 :: CardDef
riseToTheOccasion3 =
  ( skill
      "51010"
      "Rise to the Occasion"
      [#wild, #wild]
      Survivor
  )
    { cdCardTraits = singleton Innate
    , cdCommitRestrictions = [OnlyYourTest, MinSkillTestValueDifference 1]
    , cdLevel = Just 3
    }
