module Arkham.Skill.CardDefs.TheDreamEaters where

import Arkham.Skill.CardDefs.Import
import Arkham.Keyword qualified as Keyword

daring :: CardDef
daring =
  (skill "06111" "Daring" [#wild, #wild, #wild] Guardian)
    { cdCardTraits = singleton Innate
    , cdCommitRestrictions = [OnlyTestWithActions [#fight, #evade]]
    , cdCommitTrigger = True
    }

essenceOfTheDream :: CardDef
essenceOfTheDream =
  (skill "06113" "Essence of the Dream" [#wild, #wild] Seeker)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdKeywords = singleton (Keyword.Bonded 1 "06112")
    , cdLevel = Nothing
    , cdWhenDiscarded = ToBonded
    }

momentum1 :: CardDef
momentum1 =
  (skill "06115" "Momentum" [#wild] Rogue)
    { cdCardTraits = singleton Practiced
    , cdLevel = Just 1
    }

selfSacrifice :: CardDef
selfSacrifice =
  (skill "06157" "Self-Sacrifice" [] Guardian)
    { cdCardTraits = singleton Spirit
    , cdCommitRestrictions = [OnlyInvestigator $ NotYou <> colocatedWithMatch You]
    }

bruteForce1 :: CardDef
bruteForce1 =
  (skill "06166" "Brute Force" [#combat] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 1
    }

threeAces1 :: CardDef
threeAces1 =
  (skill "06199" "Three Aces" [#wild] Rogue)
    { cdKeywords = singleton Keyword.Myriad
    , cdCardTraits = setFromList [Fortune, Practiced]
    , cdLevel = Just 1
    , cdCommitTrigger = True
    }

sharpVision1 :: CardDef
sharpVision1 =
  (skill "06204" "Sharp Vision" [#intellect] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 1
    }

leadership2 :: CardDef
leadership2 =
  (skill "06235" "Leadership" [#wild] Guardian)
    { cdCardTraits = singleton Practiced
    , cdLevel = Just 2
    }

daredevil2 :: CardDef
daredevil2 =
  (skill "06240" "Daredevil" [#wild] Rogue)
    { cdCardTraits = setFromList [Fortune, Practiced]
    , cdLevel = Just 2
    , cdCommitTrigger = True
    }

expeditiousRetreat1 :: CardDef
expeditiousRetreat1 =
  (skill "06246" "Expeditious Retreat" [#agility] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 1
    }

surprisingFind1 :: CardDef
surprisingFind1 =
  (skill "06278" "Surprising Find" [#wild] Seeker)
    { cdCardTraits = setFromList [Fortune, Research]
    , cdKeywords = singleton Keyword.Myriad
    , cdLevel = Just 1
    , cdOutOfPlayEffects = [InSearchEffect]
    }

theEyeOfTruth5 :: CardDef
theEyeOfTruth5 =
  (skill "06325" "The Eye of Truth" [#wild, #wild, #wild, #wild] Seeker)
    { cdCardTraits = setFromList [Spell, Practiced]
    , cdLevel = Just 5
    }

dreamParasite :: CardDef
dreamParasite =
  (skill "06331" "Dream Parasite" [#wildMinus, #wildMinus] Neutral)
    { cdCardTraits = singleton Curse
    , cdCardSubType = Just Weakness
    , cdCommitRestrictions = [MustBeCommittedToYourTest]
    , cdKeywords = singleton (Keyword.Bonded 3 "06330")
    , cdLevel = Nothing
    }
