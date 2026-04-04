module Arkham.Event.Cards.MiguelDeLaCruz where

import Arkham.Criteria (evadeOverride, fightOverride)
import Arkham.Event.Cards.Import
import Arkham.ForMovement

decoyTrap :: CardDef
decoyTrap =
  (event "60562" "Decoy Trap" 2 Survivor)
    { cdCardTraits = setFromList [Trap, Trick]
    , cdSkills = [#agility, #combat, #wild]
    , cdLimits = [LimitPerTraitPerLocation Trap 1]
    }

glassing :: CardDef
glassing =
  (event "60563" "Glassing" 2 Survivor)
    { cdCardTraits = setFromList [Insight, Trap]
    , cdSkills = [#agility, #intellect, #wild]
    , cdLimits = [LimitPerTraitPerLocation Trap 1]
    }

guerrillaTactics :: CardDef
guerrillaTactics =
  (event "60564" "Guerrilla Tactics" 1 Survivor)
    { cdCardTraits = setFromList [Tactic]
    , cdSkills = [#agility, #combat]
    , cdActions = OrCardActions [CardAction #fight, CardAction #evade]
    , cdCriteria =
        Just
          $ exists
          $ oneOf
            [ fightOverride $ EnemyAt (orConnected NotForMovement YourLocation)
            , evadeOverride $ EnemyAt (orConnected NotForMovement YourLocation)
            ]
    , cdOverrideActionPlayableIfCriteriaMet = True
    }

hiddenShelter :: CardDef
hiddenShelter =
  (event "60565" "Hidden Shelter" 1 Survivor)
    { cdCardTraits = setFromList [Supply, Trick]
    }

lieInWait :: CardDef
lieInWait =
  (event "60566" "Lie in Wait" 2 Survivor)
    { cdCardTraits = setFromList [Tactic, Trap]
    , cdSkills = [#combat, #wild, #willpower]
    , cdLimits = [LimitPerTraitPerLocation Trap 1]
    }

stalkPrey :: CardDef
stalkPrey =
  (event "60567" "Stalk Prey" 1 Survivor)
    { cdCardTraits = setFromList [Tactic]
    , cdSkills = [#agility, #intellect]
    }

fieldDressing1 :: CardDef
fieldDressing1 =
  (event "60571" "Field Dressing" 1 Survivor)
    { cdCardTraits = setFromList [Spirit]
    , cdSkills = [#willpower]
    , cdFastWindow =
        Just $ EnemyDefeated #after You ByAny (oneOf [EnemyWithTrait Creature, EnemyWithTrait Monster])
    , cdLevel = Just 1
    }

guerrillaTactics2 :: CardDef
guerrillaTactics2 =
  (event "60576" "Guerrilla Tactics" 1 Survivor)
    { cdCardTraits = setFromList [Tactic]
    , cdSkills = [#agility, #combat, #wild]
    , cdActions = OrCardActions [CardAction #fight, CardAction #evade]
    , cdCriteria =
        Just
          $ exists
          $ oneOf
            [ fightOverride $ EnemyAt (orConnected NotForMovement YourLocation)
            , evadeOverride $ EnemyAt (orConnected NotForMovement YourLocation)
            ]
    , cdOverrideActionPlayableIfCriteriaMet = True
    , cdLevel = Just 2
    }

respite2 :: CardDef
respite2 =
  (event "60577" "Respite" 1 Survivor)
    { cdCardTraits = setFromList [Spirit]
    , cdSkills = [#wild, #wild]
    , cdCriteria = Just $ notExists (EnemyAt YourLocation)
    , cdLevel = Just 2
    }

ropeTrap2 :: CardDef
ropeTrap2 =
  (event "60578" "Rope Trap" 2 Survivor)
    { cdCardTraits = setFromList [Trap, Trick]
    , cdSkills = [#combat, #intellect]
    , cdLevel = Just 2
    , cdLimits = [LimitPerTraitPerLocation Trap 1]
    }

makeshiftBomb3 :: CardDef
makeshiftBomb3 =
  (event "60581" "Makeshift Bomb" 3 Survivor)
    { cdCardTraits = setFromList [Trap]
    , cdSkills = [#agility, #combat, #wild]
    , cdLevel = Just 3
    , cdLimits = [LimitPerTraitPerLocation Trap 1]
    }
