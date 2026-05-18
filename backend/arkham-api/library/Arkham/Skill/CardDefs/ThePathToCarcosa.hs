module Arkham.Skill.CardDefs.ThePathToCarcosa where

import Arkham.Skill.CardDefs.Import

theHomeFront :: CardDef
theHomeFront =
  signature "03001"
    $ (skill "03007" "The Home Front" (replicate 4 #combat) Neutral)
      { cdCardTraits = setFromList [Practiced, Expert]
      }

resourceful :: CardDef
resourceful =
  (skill "03039" "Resourceful" [#intellect, #combat, #agility] Survivor)
    { cdCardTraits = singleton Innate
    }

sayYourPrayers :: CardDef
sayYourPrayers =
  (skill "03116" "Say Your Prayers" [#willpower, #willpower, #willpower, #willpower] Neutral)
    { cdCardTraits = singleton Desperate
    , cdCommitRestrictions =
        [ MaxOnePerTest
        , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
        ]
    }

desperateSearch :: CardDef
desperateSearch =
  (skill "03117" "Desperate Search" [#intellect, #intellect, #intellect, #intellect] Neutral)
    { cdCardTraits = singleton Desperate
    , cdCommitRestrictions =
        [ MaxOnePerTest
        , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
        ]
    }

recklessAssault :: CardDef
recklessAssault =
  (skill "03118" "Reckless Assault" [#combat, #combat, #combat, #combat] Neutral)
    { cdCardTraits = singleton Desperate
    , cdCommitRestrictions =
        [ MaxOnePerTest
        , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
        ]
    }

runForYourLife :: CardDef
runForYourLife =
  (skill "03119" "Run For Your Life" [#agility, #agility, #agility, #agility] Neutral)
    { cdCardTraits = singleton Desperate
    , cdCommitRestrictions =
        [ MaxOnePerTest
        , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
        ]
    }

inspiringPresence :: CardDef
inspiringPresence =
  (skill "03228" "Inspiring Presence" [#willpower, #intellect, #combat] Guardian)
    { cdCardTraits = singleton Innate
    }

eureka :: CardDef
eureka =
  (skill "03231" "Eureka!" [#willpower, #intellect, #agility] Seeker)
    { cdCardTraits = singleton Innate
    }

watchThis :: CardDef
watchThis =
  (skill "03233" "\"Watch this!\"" [#willpower, #combat, #agility] Rogue)
    { cdCardTraits = singleton Gambit
    , cdCommitRestrictions = [OnlyYourTest]
    , cdAlternateCardCodes = ["60371"]
    }

torrentOfPower :: CardDef
torrentOfPower =
  (skill "03235" "Torrent of Power" [#wild] Mystic)
    { cdCardTraits = singleton Practiced
    , cdAlternateCardCodes = ["60469"]
    }

notWithoutAFight :: CardDef
notWithoutAFight =
  (skill "03272" "\"Not without a fight!\"" [#willpower, #combat, #agility] Survivor)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [SelfCanCommitWhen $ InvestigatorEngagedWith AnyEnemy]
    }

sealOfTheElderSign5 :: CardDef
sealOfTheElderSign5 =
  (skill "03312" "Seal of the Elder Sign" [#wild] Mystic)
    { cdCardTraits = setFromList [Spell, Expert]
    , cdLevel = Just 5
    }
