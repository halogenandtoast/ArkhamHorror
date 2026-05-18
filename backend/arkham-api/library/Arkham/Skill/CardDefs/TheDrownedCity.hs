module Arkham.Skill.CardDefs.TheDrownedCity where

import Arkham.Skill.CardDefs.Import
import Arkham.Keyword qualified as Keyword

determined :: CardDef
determined =
  signature "11001"
    $ (skill "11002" "Determined" [#wild] Neutral)
      { cdCardTraits = setFromList [Innate]
      }

grimResolve :: CardDef
grimResolve =
  signature "11017"
    $ (skill "11018" "Grim Resolve" [#wild] Neutral)
      { cdCardTraits = setFromList [Innate, Developed]
      }

hardboiled :: CardDef
hardboiled =
  (skill "11025" "Hardboiled" [#combat, #wild] Guardian)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdOutOfPlayEffects = [InHandEffect]
    }

inspiringPresence2 :: CardDef
inspiringPresence2 =
  (skill "11029" "Inspiring Presence" [#willpower, #intellect, #combat, #wild] Guardian)
    { cdCardTraits = setFromList [Developed, Innate]
    , cdLevel = Just 2
    }

quickWitted1 :: CardDef
quickWitted1 =
  (skill "11042" "Quick-Witted" [#intellect, #agility] Seeker)
    { cdCardTraits = singleton Innate
    , cdKeywords = setFromList [Keyword.Myriad]
    , cdLevel = Just 1
    , cdOutOfPlayEffects = [InHandEffect]
    }

crackShot :: CardDef
crackShot =
  (skill "11057" "Crack Shot" [#wild, #wild, #wild] Rogue)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions =
        [ OnlySkillTestSource
            $ SourceIsAsset
            $ AssetControlledBy You
            <> mapOneOf AssetWithTrait [Firearm, Ranged]
        ]
    }

watchThis3 :: CardDef
watchThis3 =
  (skill "11061" "\"Watch this!\"" [#wild, #wild, #wild] Rogue)
    { cdCardTraits = singleton Gambit
    , cdCommitRestrictions = [OnlyYourTest]
    , cdLevel = Just 3
    }

enraptured2 :: CardDef
enraptured2 =
  (skill "11077" "Enraptured" [#intellect, #wild] Mystic)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdLevel = Just 2
    , cdCommitRestrictions = [MaxOnePerTest]
    }

contemplative :: CardDef
contemplative =
  (skill "11088" "Contemplative" [] Survivor)
    { cdCardTraits = setFromList [Innate]
    , cdOutOfPlayEffects = [InHandEffect]
    , cdCommitRestrictions =
        [ OnlySkillTest
            $ oneOf
              [ WhileParleying <> SkillTestAt (orConnected NotForMovement YourLocation)
              , WhileInvestigating (orConnected NotForMovement YourLocation)
              ]
        ]
    }

lastChance3 :: CardDef
lastChance3 =
  (skill "11093" "Last Chance" [#wild, #wild, #wild, #wild, #wild, #wild] Survivor)
    { cdCardTraits = singleton Gambit
    , cdCommitRestrictions = [OnlyCardCommittedToTest]
    , cdOutOfPlayEffects = [InHandEffect]
    , cdLevel = Just 3
    }

confidence :: CardDef
confidence =
  (skill "11097" "Confidence" [#wild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

dreamsOfTheClay1 :: CardDef
dreamsOfTheClay1 =
  (skill "11100" "Dreams of the Clay" [#wild] Neutral)
    { cdCardTraits = setFromList [Innate, Augury]
    , cdCommitRestrictions = [OnlySkillTestSource (SourceIsTreacheryEffect AnyTreachery)]
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Artist, Clairvoyant, Dreamer, Performer]]
    , cdLevel = Just 1
    }

inquisitive1 :: CardDef
inquisitive1 =
  (skill "11101" "Inquisitive" [#wild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Assistant, Miskatonic, Scholar]]
    , cdLevel = Just 1
    }

doubleDown2 :: CardDef
doubleDown2 =
  (skill "11107" "Double Down" [#wild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Criminal, Entrepreneur, Socialite]]
    , cdLevel = Just 2
    , cdCommitTrigger = True
    }

memoriesOfAnotherLife5 :: CardDef
memoriesOfAnotherLife5 =
  (skill "11125" "Memories of Another Life" [#wild, #wild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Artist, Believer, Dreamer, Sorcerer]]
    , cdLevel = Just 5
    }
