module Arkham.Skill.CardDefs.TheInnsmouthConspiracy where

import Arkham.Skill.CardDefs.Import

whispersFromTheDeep :: CardDef
whispersFromTheDeep =
  (skill "07009" "Whispers from the Deep" [#wildMinus] Neutral)
    { cdCardTraits = singleton Curse
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdOutOfPlayEffects = [InHandEffect]
    }

planOfAction :: CardDef
planOfAction =
  (skill "07024" "Plan of Action" [#wild] Seeker)
    { cdCardTraits = setFromList [Practiced]
    }

promiseOfPower :: CardDef
promiseOfPower =
  (skill "07032" "Promise of Power" [#wild, #wild, #wild, #wild] Mystic)
    { cdCardTraits = setFromList [Practiced, Cursed]
    , cdCommitTrigger = True
    }

predestined :: CardDef
predestined =
  (skill "07035" "Predestined" [] Survivor)
    { cdCardTraits = setFromList [Fortune, Blessed]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

beloved :: CardDef
beloved =
  (skill "07036" "Beloved" [#willpower, #agility, #wild] Survivor)
    { cdCardTraits = setFromList [Innate, Blessed]
    }

skeptic1 :: CardDef
skeptic1 =
  (skill "07115" "Skeptic" [#wild] Rogue)
    { cdCardTraits = setFromList [Practiced]
    , cdLevel = Just 1
    }

unrelenting1 :: CardDef
unrelenting1 =
  (skill "07196" "Unrelenting" [#wild] Survivor)
    { cdCardTraits = singleton Practiced
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 1
    , cdCommitTrigger = True
    }

signumCrucis2 :: CardDef
signumCrucis2 =
  (skill "07197" "Signum Crucis" [#wild] Survivor)
    { cdCardTraits = setFromList [Practiced, Blessed]
    , cdCommitRestrictions = [OnlyYourTest, MinSkillTestValueDifference 1]
    , cdLevel = Just 2
    , cdCommitTrigger = True
    }

fey1 :: CardDef
fey1 =
  (skill "07222" "Fey" [#willpower, #wild, #wild] Seeker)
    { cdCardTraits = setFromList [Innate, Cursed]
    , cdLevel = Just 1
    , cdCommitTrigger = True
    }

justifyTheMeans3 :: CardDef
justifyTheMeans3 =
  (skill "07306" "Justify the Means" [] Rogue)
    { cdCardTraits = setFromList [Practiced, Cursed]
    , cdLevel = Just 3
    , cdCommitTrigger = True
    }

nauticalProwess :: CardDef
nauticalProwess =
  signature "07005"
    $ (skill "98014" "Nautical Prowess" [#willpower, #intellect, #wild] Neutral)
      { cdCardTraits = setFromList [Innate, Developed]
      }
