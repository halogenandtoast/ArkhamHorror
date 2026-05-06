module Arkham.Event.Cards.TommyMuldoon2 where

import Arkham.Event.Cards.Import
import Arkham.Token (Token (Ammo))

makeEmSing :: CardDef
makeEmSing =
  (event "60160" "\"Make 'em sing\"" 2 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdSkills = [#combat, #intellect]
    , cdActions = #parley
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> CanParleyEnemy You
    }

bounty :: CardDef
bounty =
  (event "60161" "Bounty" 0 Guardian)
    { cdCardTraits = setFromList [Fortune]
    , cdSkills = [#combat]
    , cdCriteria = Just $ exists $ affectsColocatedMatch You <> can.gain.resources
    , cdFastWindow = Just $ IfEnemyDefeated #after Anyone ByAny $ EnemyWasAt YourLocation
    }

customGrip :: CardDef
customGrip =
  (event "60162" "Custom Grip" 1 Guardian)
    { cdCardTraits = setFromList [Upgrade]
    , cdSkills = [#intellect]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ AssetControlledBy You <> #firearm
    }

ironSights :: CardDef
ironSights =
  (event "60163" "Iron Sights" 1 Guardian)
    { cdCardTraits = setFromList [Item, Upgrade]
    , cdSkills = [#wild]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ AssetControlledBy You <> #firearm
    }

physicalFitness :: CardDef
physicalFitness =
  (event "60164" "Physical Fitness" 2 Guardian)
    { cdCardTraits = setFromList [Spirit]
    , cdSkills = [#agility, #combat]
    , cdActions = #move
    }

restrained :: CardDef
restrained =
  (event "60165" "Restrained" 1 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdSkills = [#agility, #willpower]
    , cdFastWindow = Just $ EnemyAttacks #after You AnyEnemyAttack NonEliteEnemy
    }

stakeout :: CardDef
stakeout =
  (event "60166" "Stakeout" 2 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdSkills = [#intellect, #intellect]
    , cdActions = #investigate
    }

extendedBarrel1 :: CardDef
extendedBarrel1 =
  (event "60172" "Extended Barrel" 1 Guardian)
    { cdCardTraits = setFromList [Item, Upgrade]
    , cdSkills = [#intellect]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ AssetControlledBy You <> #firearm
    , cdLevel = Just 1
    }

onTheBeat1 :: CardDef
onTheBeat1 =
  (event "60173" "On the Beat" 2 Guardian)
    { cdCardTraits = setFromList [Tactic, Police]
    , cdSkills = [#intellect, #wild, #willpower]
    , cdFastWindow = Just $ TurnBegins #after You
    , cdLevel = Just 1
    }

physicalFitness2 :: CardDef
physicalFitness2 =
  (event "60174" "Physical Fitness" 1 Guardian)
    { cdCardTraits = setFromList [Spirit]
    , cdSkills = [#agility, #combat]
    , cdActions = #move
    , cdLevel = Just 2
    }

stockAmmoReload2 :: CardDef
stockAmmoReload2 =
  (event "60175" "Stock Ammo Reload" 2 Guardian)
    { cdCardTraits = setFromList [Supply]
    , cdSkills = [#willpower]
    , cdCriteria = Just $ exists $ AssetControlledBy You <> AssetWithUseType Ammo
    , cdLevel = Just 2
    }

stakeout3 :: CardDef
stakeout3 =
  (event "60179" "Stakeout" 2 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdSkills = [#agility, #intellect]
    , cdActions = #investigate
    , cdLevel = Just 3
    }
