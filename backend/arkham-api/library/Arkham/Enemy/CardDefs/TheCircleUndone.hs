{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.TheCircleUndone where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

hoods :: CardDef
hoods =
  (weakness "05017" "Hoods")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

anetteMason :: CardDef
anetteMason =
  unique
    $ (enemy "05057" ("Anette Mason" <:> "The High Priestess") TheWitchingHour 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Humanoid, Witch, Elite]
      , cdKeywords = singleton Keyword.Retaliate
      , cdVictoryPoints = Just 2
      }

josefMeiger :: CardDef
josefMeiger =
  unique
    $ doubleSided "05085b"
    $ (enemy "05085" ("Josef Meiger" <:> "Lodge Host") AtDeathsDoorstep 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight, Elite]
      , cdKeywords = singleton Keyword.Retaliate
      , cdVictoryPoints = Just 2
      }

theSpectralWatcher :: CardDef
theSpectralWatcher =
  unique
    $ (enemy "05086" ("The Spectral Watcher" <:> "You Are Its Prey") TheWatcher 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [AncientOne, Spectral, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
      }

piperOfAzathoth :: CardDef
piperOfAzathoth =
  (enemy "05088" "Piper of Azathoth" AgentsOfAzathoth 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 2
    , cdHealth = health 7
    , cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }

covenInitiate :: CardDef
covenInitiate =
  (enemy "05090" "Coven Initiate" AnettesCoven 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Witch]
    , cdRevelation = IsRevelation
    }

priestessOfTheCoven :: CardDef
priestessOfTheCoven =
  (enemy "05091" "Priestess of the Coven" AnettesCoven 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Witch]
    , cdKeywords = singleton Keyword.Retaliate
    }

lodgeNeophyte :: CardDef
lodgeNeophyte =
  (enemy "05095" "Lodge Neophyte" SilverTwilightLodge 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Aloof
    }

keeperOfSecrets :: CardDef
keeperOfSecrets =
  (enemy "05096" "Keeper of Secrets" SilverTwilightLodge 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    }

netherMist :: CardDef
netherMist =
  (enemy "05100" "Nether Mist" SpectralPredators 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Spectral]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

shadowHound :: CardDef
shadowHound =
  (enemy "05101" "Shadow Hound" SpectralPredators 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Spectral]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

wraith :: CardDef
wraith =
  (enemy "05103" "Wraith" TrappedSpirits 2)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist, Spectral]
    , cdKeywords = singleton Keyword.Hunter
    }

brownJenkin :: CardDef
brownJenkin =
  unique
    $ (enemy "05148" ("Brown Jenkin" <:> "The Witch's Familiar") TheSecretName 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 1
      , cdEvade = evade 4
      , cdHealth = health 1
      , cdCardTraits = setFromList [Creature, Familiar, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      }

nahab :: CardDef
nahab =
  unique
    $ (enemy "05149" ("Nahab" <:> "She Who Signed the Black Book") TheSecretName 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 1
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 1
      , cdCardTraits = setFromList [Monster, Geist, Witch, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      }

heretic_A :: CardDef
heretic_A =
  doubleSided "05178b"
    $ (enemy "05178a" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

heretic_C :: CardDef
heretic_C =
  doubleSided "05178d"
    $ (enemy "05178c" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

heretic_E :: CardDef
heretic_E =
  doubleSided "05178f"
    $ (enemy "05178e" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

heretic_G :: CardDef
heretic_G =
  doubleSided "05178h"
    $ (enemy "05178g" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      , cdOutOfPlayEffects = [InDiscardEffect]
      }

heretic_I :: CardDef
heretic_I =
  doubleSided "05178j"
    $ (enemy "05178i" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

heretic_K :: CardDef
heretic_K =
  doubleSided "05178l"
    $ (enemy "05178k" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

vengefulWitch :: CardDef
vengefulWitch =
  (enemy "05179" "Vengeful Witch" TheWagesOfSin 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Witch]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

malevolentSpirit :: CardDef
malevolentSpirit =
  (enemy "05180" "Malevolent Spirit" TheWagesOfSin 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist, Spectral]
    }

nathanWickMasterOfInitiation :: CardDef
nathanWickMasterOfInitiation =
  unique
    $ doubleSided "05217b"
    $ (enemy "05217a" ("Nathan Wick" <:> "Master of Initiation") ForTheGreaterGood 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight, Elite]
      , cdKeywords = singleton Keyword.Retaliate
      , cdVictoryPoints = Just 1
      }

nathanWickMasterOfIndoctrination :: CardDef
nathanWickMasterOfIndoctrination =
  unique
    $ doubleSided "05217a"
    $ (enemy "05217b" ("Nathan Wick" <:> "Master of Indoctrination") ForTheGreaterGood 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight, Elite]
      , cdKeywords = singleton Keyword.Alert
      , cdVictoryPoints = Just 1
      }

lodgeJailor :: CardDef
lodgeJailor =
  (enemy "05218" "Lodge Jailor" ForTheGreaterGood 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Aloof
    }

cellKeeper :: CardDef
cellKeeper =
  (enemy "05219" "Cell Keeper" ForTheGreaterGood 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Alert
    }

summonedBeast :: CardDef
summonedBeast =
  (enemy "05220" ("Summoned Beast" <:> "Guardian of the Trap") ForTheGreaterGood 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 2
    , cdHealth = healthPerInvestigator 6
    , cdCardTraits = setFromList [Monster, SilverTwilight, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Hunter]
    , cdVictoryPoints = Just 2
    }

knightOfTheInnerCircle :: CardDef
knightOfTheInnerCircle =
  (enemy "05221" "Knight of the Inner Circle" ForTheGreaterGood 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof, Keyword.Hunter]
    }

knightOfTheOuterVoid :: CardDef
knightOfTheOuterVoid =
  (enemy "05222" "Knight of the Outer Void" ForTheGreaterGood 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Peril, Keyword.Retaliate]
    , cdRevelation = IsRevelation
    }

gavriellaMizrah :: CardDef
gavriellaMizrah =
  unique
    $ doubleSided "05262"
    $ (enemy "05262b" ("Gavriella Mizrah" <:> "You're Next") UnionAndDisillusion 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 5
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

jeromeDavids :: CardDef
jeromeDavids =
  unique
    $ doubleSided "05263"
    $ (enemy "05263b" ("Jerome Davids" <:> "Starved for Answers") UnionAndDisillusion 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

pennyWhite :: CardDef
pennyWhite =
  unique
    $ doubleSided "05264"
    $ (enemy "05264b" ("Penny White" <:> "Tragic Loss") UnionAndDisillusion 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

valentinoRivas :: CardDef
valentinoRivas =
  unique
    $ doubleSided "05265"
    $ (enemy "05265b" ("Valentino Rivas" <:> "Ripped Asunder") UnionAndDisillusion 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

whippoorwillUnionAndDisillusion :: CardDef
whippoorwillUnionAndDisillusion =
  (enemy "05266" "Whippoorwill" UnionAndDisillusion 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

spectralRaven :: CardDef
spectralRaven =
  (enemy "05267" "Spectral Raven" UnionAndDisillusion 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Spectral]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
    }

anetteMasonReincarnatedEvil :: CardDef
anetteMasonReincarnatedEvil =
  doubleSided "05286"
    $ (enemy "05286b" ("Anette Mason" <:> "Reincarnated Evil") MusicOfTheDamned 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 5
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Witch, Servitor, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

carlSanfordDeathlessFanatic :: CardDef
carlSanfordDeathlessFanatic =
  doubleSided "05288"
    $ (enemy "05288b" ("Carl Sanford" <:> "Deathless Fanatic") SecretsOfTheUniverse 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, SilverTwilight, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

lodgeEnforcer :: CardDef
lodgeEnforcer =
  (enemy "05309" "Lodge Enforcer" SecretsOfTheUniverse 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Retaliate
    , cdVictoryPoints = Just 1
    }

witnessOfChaos :: CardDef
witnessOfChaos =
  (enemy "05311" "Witness of Chaos" MusicOfTheDamned 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Witch]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

mindlessDancer :: CardDef
mindlessDancer =
  (enemy "05341" "Mindless Dancer" BeforeTheBlackThrone 3)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 6
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    }

azathoth :: CardDef
azathoth =
  (enemy "05346" ("Azathoth" <:> "The Primal Chaos") BeforeTheBlackThrone 1)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 3
    , cdCardTraits = setFromList [AncientOne, Elite]
    , cdUnique = True
    }
