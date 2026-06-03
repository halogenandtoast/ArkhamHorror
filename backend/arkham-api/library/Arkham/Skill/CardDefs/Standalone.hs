module Arkham.Skill.CardDefs.Standalone where

import Arkham.Skill.CardDefs.Import

overpower2 :: CardDef
overpower2 =
  (skill "60126" "Overpower" [#combat, #combat, #combat] Guardian)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 2
    }

perception2 :: CardDef
perception2 =
  (skill "60228" "Perception" [#intellect, #intellect, #intellect] Seeker)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 2
    }

anythingYouCanDoBetter :: CardDef
anythingYouCanDoBetter =
  signature "60301"
    $ ( skill
          "60302"
          "Anything You Can Do, Better"
          [#wild, #wild, #wild, #wild, #wild, #wild]
          Rogue
      )
      { cdCardTraits = setFromList [Innate, Developed]
      , cdCommitRestrictions = [OnlyYourTest]
      }

knowTheExit :: CardDef
knowTheExit =
  signature "60351"
    $ (skill "60352" "Know the Exit" [#wild] Rogue)
      { cdCardTraits = singleton Practiced
      }

knowTheLine :: CardDef
knowTheLine =
  signature "60351"
    $ (skill "60353" "Know the Line" [#wild] Rogue)
      { cdCardTraits = singleton Practiced
      }

knowTheScene :: CardDef
knowTheScene =
  signature "60351"
    $ (skill "60354" "Know the Scene" [#wild] Rogue)
      { cdCardTraits = singleton Practiced
      }

arrogance :: CardDef
arrogance =
  (skill "60303" "Arrogance" [#wildMinus] Neutral)
    { cdCardTraits = singleton Flaw
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdCommitRestrictions = [MustBeCommittedToYourTest]
    }

reckless :: CardDef
reckless =
  (skill "60304" "Reckless" [] Neutral)
    { cdCardTraits = singleton Flaw
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdCommitRestrictions = [OnlyYourTest, OnlyCardCommittedToTest]
    , cdOutOfPlayEffects = [InHandEffect]
    }

nimble :: CardDef
nimble =
  (skill "60317" "Nimble" [#agility] Rogue)
    { cdCardTraits = singleton Innate
    }

daredevil :: CardDef
daredevil =
  (skill "60318" "Daredevil" [#wild] Rogue)
    { cdCardTraits = setFromList [Fortune, Practiced]
    , cdCommitTrigger = True
    }

manualDexterity2 :: CardDef
manualDexterity2 =
  (skill "60325" "Manual Dexterity" [#agility, #agility, #agility] Rogue)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 2
    }

copycat3 :: CardDef
copycat3 =
  (skill "60330" "Copycat" [#wild] Rogue)
    { cdCardTraits = singleton Gambit
    , cdLevel = Just 3
    , cdCommitTrigger = True
    }

prescient :: CardDef
prescient =
  (skill "60419" "Prescient" [#willpower] Mystic)
    { cdCardTraits = setFromList [Practiced, Augury]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdCommitTrigger = True
    }

guts2 :: CardDef
guts2 =
  (skill "60424" "Guts" [#willpower, #willpower, #willpower] Mystic)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 2
    }

neitherRainNorSnow :: CardDef
neitherRainNorSnow =
  signature "60501"
    $ (skill "60502" "Neither Rain nor Snow" [#wild, #wild, #wild] Survivor)
      { cdCardTraits = setFromList [Innate, Developed]
      }

unexpectedCourage2 :: CardDef
unexpectedCourage2 =
  (skill "60526" "Unexpected Courage" [#wild, #wild] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 2
    }

easyStreet :: CardDef
easyStreet =
  (skill "60369" "Easy Street" [#wild] Rogue)
    { cdCardTraits = singleton Favor
    , cdCommitRestrictions = [OnlyYourTest, MaxOnePerTest]
    }

outTheDoor :: CardDef
outTheDoor =
  (skill "60370" "Out the Door" [#agility, #wild] Rogue)
    { cdCardTraits = singleton Gambit
    , cdCommitTrigger = True
    }

outTheDoor1 :: CardDef
outTheDoor1 =
  (skill "60373" "Out the Door" [#agility, #wild] Rogue)
    { cdCardTraits = singleton Gambit
    , cdLevel = Just 1
    , cdCommitTrigger = True
    }

contingency3 :: CardDef
contingency3 =
  (skill "60383" "Contingency" [#wild, #wild] Rogue)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdLevel = Just 3
    }

adaptAndOvercome :: CardDef
adaptAndOvercome =
  (skill "60167" "Adapt and Overcome" [#agility, #combat, #wild] Guardian)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [OnlyTestWithActions [#fight, #evade]]
    }

armedToTheTeeth :: CardDef
armedToTheTeeth =
  (skill "60168" "Armed to the Teeth" [#agility, #combat, #willpower] Guardian)
    { cdCardTraits = setFromList [Practiced]
    }

indomitable3 :: CardDef
indomitable3 =
  (skill "60180" "Indomitable" [#combat, #wild, #willpower] Guardian)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdLevel = Just 3
    }

establishMotive :: CardDef
establishMotive =
  (skill "60268" "Establish Motive" [#wild] Seeker)
    { cdCardTraits = singleton Practiced
    }

literaryAnalysis :: CardDef
literaryAnalysis =
  (skill "60269" "Literary Analysis" [#intellect, #intellect] Seeker)
    { cdCardTraits = singleton Practiced
    }

bloodCurse :: CardDef
bloodCurse =
  (skill "60467" "Blood Curse" [#wild, #wild, #wild, #wild] Mystic)
    { cdCardTraits = setFromList [Spell, Cursed]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

cosmicGuidance :: CardDef
cosmicGuidance =
  (skill "60468" "Cosmic Guidance" [#willpower] Mystic)
    { cdCardTraits = singleton Augury
    }

eldritchWhispers1 :: CardDef
eldritchWhispers1 =
  (skill "60471" "Eldritch Whispers" [#willpower, #willpower] Mystic)
    { cdCardTraits = singleton Innate
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 1
    , cdCommitTrigger = True
    }

bloodCurse3 :: CardDef
bloodCurse3 =
  (skill "60477" "Blood Curse" [#wild, #wild, #wild, #wild, #wild] Mystic)
    { cdCardTraits = setFromList [Spell, Cursed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 3
    }

doOrDie :: CardDef
doOrDie =
  (skill "60568" "Do or Die" [#agility, #intellect, #willpower] Survivor)
    { cdCardTraits = singleton Fortune
    }

onTheBrink :: CardDef
onTheBrink =
  (skill "60569" "On the Brink" [#wild] Survivor)
    { cdCardTraits = setFromList [Gambit, Desperate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

rough1 :: CardDef
rough1 =
  (skill "60572" "Rough" [#combat, #wild] Survivor)
    { cdCardTraits = singleton Innate
    , cdLevel = Just 1
    }

timelyIntervention3 :: CardDef
timelyIntervention3 =
  (skill "60582" "Timely Intervention" [#wild, #wild, #wild] Survivor)
    { cdCardTraits = singleton Fortune
    , cdCommitRestrictions = [MaxOnePerTest, CanCommitAfterRevealingTokens]
    , cdLevel = Just 3
    }
