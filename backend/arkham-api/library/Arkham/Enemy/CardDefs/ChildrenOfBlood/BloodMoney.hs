module Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

priscillaThomas :: CardDef
priscillaThomas =
  (enemy "13083" ("Priscilla Thomas" <:> "Skeptical Business Partner") BloodMoney 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdHealth = health 3
    , cdEvade = evade 2
    , cdCardTraits = setFromList [Humanoid, Civilian, Elite]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Doomed, Keyword.Elusive]
    , cdUnique = True
    }

howardWilkesFirstChildOfZburamoarte :: CardDef
howardWilkesFirstChildOfZburamoarte =
  (enemy "13084" ("Howard Wilkes" <:> "First Child of Zburamoarte") BloodMoney 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdHealth = health 4
    , cdEvade = evade 3
    , cdCardTraits = setFromList [Humanoid, Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Predator]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

howardWilkesTooFarGone :: CardDef
howardWilkesTooFarGone =
  (enemy "13085" ("Howard Wilkes" <:> "Too Far Gone") BloodMoney 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdHealth = health 6
    , cdEvade = evade 3
    , cdCardTraits = setFromList [Humanoid, Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Predator, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

howardWilkesUltimatePredator :: CardDef
howardWilkesUltimatePredator =
  (enemy "13086" ("Howard Wilkes" <:> "Ultimate Predator") BloodMoney 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdHealth = health 6
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Humanoid, Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Predator, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

juliaSternFirstVictimOfNewHorizons :: CardDef
juliaSternFirstVictimOfNewHorizons =
  (enemy "13087" ("Julia Stern" <:> "First Victim of New Horizons") BloodMoney 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdHealth = health 4
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Humanoid, Monster, Elite]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

juliaSternOutForBlood :: CardDef
juliaSternOutForBlood =
  (enemy "13088" ("Julia Stern" <:> "Out for Blood") BloodMoney 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdHealth = health 6
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Humanoid, Monster, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Elusive, Keyword.Hunter]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

juliaSternChildOfVengeance :: CardDef
juliaSternChildOfVengeance =
  (enemy "13089" ("Julia Stern" <:> "Child of Vengeance") BloodMoney 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdHealth = health 6
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Humanoid, Monster, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Elusive, Keyword.Hunter, Keyword.Predator]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

suspiciousGuest :: CardDef
suspiciousGuest =
  (enemy "13090" "Suspicious Guest" BloodMoney 6)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdHealth = health 1
    , cdEvade = evade 1
    , cdCardTraits = setFromList [Humanoid, Civilian]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Doomed]
    }

childOfBlood :: CardDef
childOfBlood =
  (enemy "13091" "Child of Blood" BloodMoney 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = singleton Keyword.Hunter
    }
