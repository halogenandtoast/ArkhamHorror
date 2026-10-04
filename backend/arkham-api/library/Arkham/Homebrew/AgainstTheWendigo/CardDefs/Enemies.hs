{- | Against the Wendigo's enemies.

Printed health is the number on the card; the "+N [per_investigator] health"
line below several of them is an ability, not printed health, so it lives in the
entity's 'HasModifiersFor' rather than in a 'StaticWithPerPlayer'.
-}
module Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.AgainstTheWendigo.Sets qualified as Set
import Arkham.Homebrew.AgainstTheWendigo.Traits
import Arkham.Keyword qualified as Keyword

-- | The back of agenda 2. It only ever enters play when that agenda advances.
bestialCreature :: CardDef
bestialCreature =
  (enemy ":against-the-wendigo:003b" "Bestial Creature" Set.HanninahValley 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Indigenous, Wendigo]
    , cdFight = fight 4
    , cdHealth = health 3
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdOtherSide = Just ":against-the-wendigo:003"
    }

deer :: CardDef
deer =
  (enemy ":against-the-wendigo:037" "Deer" Set.HanninahValley 2)
    { cdCardTraits = setFromList [Animal, Wild]
    , cdFight = fight 2
    , cdHealth = health 3
    , cdEvade = evade 4
    , cdKeywords = singleton Keyword.Aloof
    }

wildIndigenous :: CardDef
wildIndigenous =
  (enemy ":against-the-wendigo:042" "Wild Indigenous" Set.HanninahValley 2)
    { cdCardTraits = setFromList [Humanoid, Indigenous]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }

angrySarceeMen :: CardDef
angrySarceeMen =
  (enemy ":against-the-wendigo:043" "Angry Sarcee Men" Set.HanninahValley 1)
    { cdCardTraits = setFromList [Humanoid, Sarcee]
    , cdFight = fight 3
    , cdHealth = health 3
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 2
    , cdKeywords = singleton Keyword.Aloof
    }

wolves :: CardDef
wolves =
  (enemy ":against-the-wendigo:045" "Wolves" Set.HanninahValley 2)
    { cdCardTraits = setFromList [Animal, Wild]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 5
    , cdHealthDamage = healthDamage 1
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

bear :: CardDef
bear =
  (enemy ":against-the-wendigo:047" "Bear" Set.HanninahValley 1)
    { cdCardTraits = setFromList [Animal, Wild]
    , cdFight = fight 4
    , cdHealth = health 4
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 2
    , cdKeywords = singleton Keyword.Massive
    }

terrifiedPoliceman :: CardDef
terrifiedPoliceman =
  (enemy ":against-the-wendigo:048" "Terrified Policeman" Set.HanninahValley 1)
    { cdCardTraits = setFromList [Humanoid, Police]
    , cdFight = fight 3
    , cdHealth = health 3
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdKeywords = singleton Keyword.Aloof
    , cdVictoryPoints = Just 1
    }

theWendigo :: CardDef
theWendigo =
  (enemy ":against-the-wendigo:054" "The Wendigo" Set.WendigosMyth 1)
    { cdCardTraits = setFromList [Wendigo, Monster, Elite]
    , cdFight = fight 5
    , cdHealth = health 4
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 2
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Hunter]
    , cdVictoryPoints = Just 3
    , cdUnique = True
    }

-- The two students who did not come back as people. Both are story-card backs.

bernardEpstein :: CardDef
bernardEpstein =
  (enemy ":against-the-wendigo:024b" "Bernard Epstein" Set.HanninahValley 1)
    { cdCardTraits = setFromList [Wendigo, Humanoid, Monster, Elite]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdUnique = True
    , cdOtherSide = Just ":against-the-wendigo:024"
    }

sylviaDavidson :: CardDef
sylviaDavidson =
  ( enemy
      ":against-the-wendigo:029b"
      ("Sylvia Davidson" <:> "Possessed by an Evil Entity")
      Set.HanninahValley
      1
  )
    { cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
    , cdFight = fight 4
    , cdHealth = health 1
    , cdEvade = evade 4
    , cdSanityDamage = sanityDamage 2
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Aloof]
    , cdUnique = True
    , cdOtherSide = Just ":against-the-wendigo:029"
    }
