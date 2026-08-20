module Arkham.Enemy.CardDefs.TheDreamEaters.TheSearchForKadath where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

beingsOfIb :: CardDef
beingsOfIb =
  (enemy "06148" "Beings of Ib" TheSearchForKadath 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Elite]
    , cdVictoryPoints = Just 1
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter, Keyword.Swarming (PerPlayer 1)]
    }

catsOfUlthar :: CardDef
catsOfUlthar =
  (enemy "06145" "Cats of Ulthar" TheSearchForKadath 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Elite]
    , cdKeywords = singleton $ Keyword.Swarming (Static 2)
    , cdVictoryPoints = Just 1
    }

furtiveZoog :: CardDef
furtiveZoog =
  (enemy "06106" "Furtive Zoog" Zoogs 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Zoog]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Swarming (Static 1)]
    }

hordeOfNight :: CardDef
hordeOfNight =
  (enemy "06147" "Horde of Night" TheSearchForKadath 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Elite]
    , cdVictoryPoints = Just 1
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Swarming (PerPlayer 1)]
    }

inconspicuousZoog :: CardDef
inconspicuousZoog =
  (enemy "06108" "Inconspicuous Zoog" Zoogs 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Zoog]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Swarming (Static 2)]
    }

nightriders :: CardDef
nightriders =
  (enemy "06152" "Nightriders" TheSearchForKadath 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 5
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = singleton (Keyword.Swarming (Static 1))
    }

packOfVooniths :: CardDef
packOfVooniths =
  (enemy "06151" "Pack of Vooniths" TheSearchForKadath 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = singleton (Keyword.Swarming (Static 1))
    }

priestOfAThousandMasks :: CardDef
priestOfAThousandMasks =
  (enemy "06149" "Priest of a Thousand Masks" TheSearchForKadath 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

stalkingManticore :: CardDef
stalkingManticore =
  (enemy "06146" "Stalking Manticore" TheSearchForKadath 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [Creature, Monster, Elite]
    , cdVictoryPoints = Just 1
    }

stealthyZoog :: CardDef
stealthyZoog =
  (enemy "06107" "Stealthy Zoog" Zoogs 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 3
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Zoog]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Swarming (Static 1)]
    }

tenebrousNightgaunt :: CardDef
tenebrousNightgaunt =
  (enemy "06150" "Tenebrous Nightgaunt" TheSearchForKadath 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Nightgaunt]
    , cdKeywords = singleton Keyword.Hunter
    }
