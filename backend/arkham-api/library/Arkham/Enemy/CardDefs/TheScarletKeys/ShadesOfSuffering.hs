module Arkham.Enemy.CardDefs.TheScarletKeys.ShadesOfSuffering where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

buriedMinerALostMemento :: CardDef
buriedMinerALostMemento =
  (enemy "09675a" "Buried Miner" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09675b"
    }

buriedMinerExhumeTheBones :: CardDef
buriedMinerExhumeTheBones =
  (enemy "09675c" "Buried Miner" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09675d"
    }

slainForemanFamilialPain :: CardDef
slainForemanFamilialPain =
  (enemy "09676c" "Slain Foreman" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09676d"
    }

slainForemanSympathyPain :: CardDef
slainForemanSympathyPain =
  (enemy "09676a" "Slain Foreman" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09676b"
    }

tzuSanNiangOutForBlood :: CardDef
tzuSanNiangOutForBlood =
  (enemy "09679b" ("Tzu San Niang" <:> "Out for Blood") ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09679"
    , cdVictoryPoints = Just 1
    }

tzuSanNiangTheLadyWithTheRedParasol :: CardDef
tzuSanNiangTheLadyWithTheRedParasol =
  (enemy "09679" ("Tzu San Niang" <:> "The Lady with the Red Parasol") ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = healthPerInvestigator 2
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Concealed TzuSanNiang (Static 2)]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09679b"
    }

uncannyShadowPlayfulShadows :: CardDef
uncannyShadowPlayfulShadows =
  (enemy "09674a" "Uncanny Shadow" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09674b"
    }

uncannyShadowTimorousShadows :: CardDef
uncannyShadowTimorousShadows =
  (enemy "09674c" "Uncanny Shadow" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09674d"
    }
