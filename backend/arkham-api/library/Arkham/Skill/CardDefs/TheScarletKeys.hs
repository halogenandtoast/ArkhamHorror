module Arkham.Skill.CardDefs.TheScarletKeys where

import Arkham.Skill.CardDefs.Import
import Arkham.Keyword qualified as Keyword

asYouWish :: CardDef
asYouWish =
  signature "09001"
    $ (skill "09002" "\"As you wish\"" [#wild, #wild, #wild] Neutral)
      { cdCardTraits = setFromList [Practiced, Expert]
      , cdCommitRestrictions = [OnlyNotYourTest]
      }

onTheMend :: CardDef
onTheMend =
  signature "09004"
    $ (skill "09006" "On the Mend" [#wild, #wild] Neutral)
      { cdCardTraits = setFromList [Innate]
      , cdCommitRestrictions = [OnlyYourTest]
      , cdWhenDiscarded = ToSetAside
      }

fightingLessons :: CardDef
fightingLessons =
  (skill "09030" "Fighting Lessons" [#combat, #agility, #wild] Guardian)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [OnlyTestWithActions [#fight, #evade]]
    , cdOutOfPlayEffects = [InHandEffect]
    }

helpingHand :: CardDef
helpingHand =
  (skill "09031" "Helping Hand" [] Guardian)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

analysis :: CardDef
analysis =
  (skill "09049" "Analysis" [#wild] Seeker)
    { cdCardTraits = setFromList [Practiced]
    }

calculatedRisk :: CardDef
calculatedRisk =
  (skill "09070" "Calculated Risk" [] Rogue)
    { cdCardTraits = setFromList [Gambit, Fated]
    , cdCommitRestrictions = [OnlyYourTest, OnlyTestDuringYourTurn, MaxOnePerTest]
    , cdCommitTrigger = True
    }

ghastlyPossession1 :: CardDef
ghastlyPossession1 =
  (skill "09090" "Ghastly Possession" [#wild] Mystic)
    { cdCardTraits = setFromList [Innate, Spell]
    , cdLevel = Just 1
    , cdCommitTrigger = True
    }

grizzled :: CardDef
grizzled =
  (skill "09101" "Grizzled" [#wild] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdOutOfPlayEffects = [InHandEffect, InDiscardEffect]
    , cdCustomizations =
        mapFromList
          [ (ChoicePlaceholder, 0)
          , (Specialist, 1)
          , (Specialist2, 2)
          , (Nemesis, 3)
          , (MythosHardened, 4)
          , (AlwaysPrepared, 5)
          ]
    }

gumption1 :: CardDef
gumption1 =
  (skill "09112" "Gumption" [] Survivor)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 1
    }
