module Arkham.Event.Cards.AndrePatel where

import Arkham.Event.Cards.Import

cleanSweep :: CardDef
cleanSweep =
  (event "60364" "Clean Sweep" 2 Rogue)
    { cdCardTraits = setFromList [Tactic, Trick]
    , cdSkills = [#agility, #intellect]
    , cdActions = [#investigate]
    }

payYourDues :: CardDef
payYourDues =
  (event "60365" "Pay Your Dues" 0 Rogue)
    { cdCardTraits = singleton Favor
    , cdSkills = [#intellect, #willpower]
    , cdActions = [#parley]
    , cdCost = Just (MatchingEnemyFieldCost (NonEliteEnemy <> EnemyAt YourLocation) EnemyRemainingHealthField)
    }

quickExit :: CardDef
quickExit =
  (event "60366" "Quick Exit" 2 Rogue)
    { cdCardTraits = setFromList [Tactic, Trick]
    , cdSkills = [#agility, #willpower]
    , cdActions = [#evade]
    }

aSuddenFall :: CardDef
aSuddenFall =
  (event "60367" "A Sudden Fall" 2 Rogue)
    { cdCardTraits = setFromList [Tactic, Trick]
    , cdSkills = [#agility, #combat]
    , cdActions = [#fight]
    }

rightUnderTheirNoses :: CardDef
rightUnderTheirNoses =
  (event "60368" "Right Under Their Noses" 2 Rogue)
    { cdCardTraits = setFromList [Trick, Illicit]
    , cdSkills = [#intellect, #willpower]
    , cdFastWindow = Just $ EnemyEvaded #after You AnyEnemy
    }

cleanSweep2 :: CardDef
cleanSweep2 =
  (event "60376" "Clean Sweep" 2 Rogue)
    { cdCardTraits = setFromList [Tactic, Trick]
    , cdSkills = [#intellect, #intellect, #wild]
    , cdActions = [#investigate]
    , cdLevel = Just 2
    }

quickExit2 :: CardDef
quickExit2 =
  (event "60377" "Quick Exit" 2 Rogue)
    { cdCardTraits = setFromList [Tactic, Trick]
    , cdSkills = [#willpower, #willpower, #wild]
    , cdActions = [#evade]
    , cdLevel = Just 2
    }

aSuddenFall2 :: CardDef
aSuddenFall2 =
  (event "60378" "A Sudden Fall" 2 Rogue)
    { cdCardTraits = setFromList [Tactic, Trick]
    , cdSkills = [#combat, #combat, #wild]
    , cdActions = [#fight]
    , cdLevel = Just 2
    }

rightUnderTheirNoses3 :: CardDef
rightUnderTheirNoses3 =
  (event "60382" "Right Under Their Noses" 2 Rogue)
    { cdCardTraits = setFromList [Trick, Illicit]
    , cdSkills = [#intellect, #willpower, #wild]
    , cdFastWindow = Just $ EnemyEvaded #after You AnyEnemy
    , cdLevel = Just 3
    }
