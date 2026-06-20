{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.TheDreamEaters where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

tonysQuarry :: CardDef
tonysQuarry =
  (weakness "06012" "Tony's Quarry")
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

watcherFromAnotherDimension :: CardDef
watcherFromAnotherDimension =
  unique
    $ (weakness "06017" "Watcher from Another Dimension")
      { cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Extradimensional]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Hunter]
      , cdRevelation = IsRevelation
      }

guardianOfTheCrystallizer :: CardDef
guardianOfTheCrystallizer =
  (weakness "06025" "Guardian of the Crystallizer")
    { cdHealth = health 3
    , cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Bonded 1 "06024", Keyword.Hunter]
    }

yourWorstNightmare :: CardDef
yourWorstNightmare =
  (basicWeakness "06038" "Your Worst Nightmare")
    { cdHealth = health 3
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    , cdDeckRestrictions = [MultiplayerOnly]
    }

kamanThah :: CardDef
kamanThah =
  doubleSided "06057b"
    $ (enemy "06057" ("Kaman-Thah" <:> "Priest of the Dreamlands") BeyondTheGatesOfSleep 1)
      { cdHealth = health 3
      , cdCardTraits = setFromList [Dreamlands, Warden, Elite]
      , cdUnique = True
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
      }

nasht :: CardDef
nasht =
  doubleSided "06058b"
    $ (enemy "06058" ("Nasht" <:> "Priest of the Dreamlands") BeyondTheGatesOfSleep 1)
      { cdHealth = health 3
      , cdCardTraits = setFromList [Dreamlands, Warden, Elite]
      , cdUnique = True
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
      }

laboringGug :: CardDef
laboringGug =
  (enemy "06060" "Laboring Gug" BeyondTheGatesOfSleep 1)
    { cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Gug]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

ancientZoog :: CardDef
ancientZoog =
  (enemy "06061" "Ancient Zoog" BeyondTheGatesOfSleep 1)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Zoog, Elite]
    , cdKeywords = singleton Keyword.Aloof
    }

suspiciousOrderly :: CardDef
suspiciousOrderly =
  (enemy "06081" "Suspicious Orderly" WakingNightmare 2)
    { cdCardTraits = setFromList [Humanoid, Staff]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

corruptedOrderly :: CardDef
corruptedOrderly =
  (enemy "06082" "Corrupted Orderly" WakingNightmare 2)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Staff, Spider]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

greyWeaver :: CardDef
greyWeaver =
  (enemy "06084" "Grey Weaver" AgentsOfAtlachNacha 2)
    { cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Spider]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

theCrawlingMist :: CardDef
theCrawlingMist =
  (enemy "06086" "The Crawling Mist" AgentsOfNyarlathotep 1)
    { cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Avatar]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

huntingGhast :: CardDef
huntingGhast =
  (enemy "06091" "Hunting Ghast" CreaturesOfTheUnderworld 3)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, Ghast]
    , cdKeywords = singleton Keyword.Hunter
    }

lumberingGug :: CardDef
lumberingGug =
  (enemy "06092" "Lumbering Gug" CreaturesOfTheUnderworld 1)
    { cdHealth = health 6
    , cdCardTraits = setFromList [Monster, Gug]
    }

spiderOfLeng :: CardDef
spiderOfLeng =
  (enemy "06101" "Spider of Leng" Spiders 1)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Spider]
    }

swarmOfSpiders :: CardDef
swarmOfSpiders =
  (enemy "06102" "Swarm of Spiders" Spiders 3)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Spider]
    , cdKeywords = setFromList [Keyword.Swarming (Static 2)]
    }

corsairOfLeng :: CardDef
corsairOfLeng =
  (enemy "06105" "Corsair of Leng" Corsairs 2)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = singleton Keyword.Alert
    }

furtiveZoog :: CardDef
furtiveZoog =
  (enemy "06106" "Furtive Zoog" Zoogs 2)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Zoog]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Swarming (Static 1)]
    }

stealthyZoog :: CardDef
stealthyZoog =
  (enemy "06107" "Stealthy Zoog" Zoogs 2)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Zoog]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Swarming (Static 1)]
    }

inconspicuousZoog :: CardDef
inconspicuousZoog =
  (enemy "06108" "Inconspicuous Zoog" Zoogs 1)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Zoog]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Swarming (Static 2)]
    }

catsOfUlthar :: CardDef
catsOfUlthar =
  (enemy "06145" "Cats of Ulthar" TheSearchForKadath 1)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Elite]
    , cdKeywords = singleton $ Keyword.Swarming (Static 2)
    , cdVictoryPoints = Just 1
    }

stalkingManticore :: CardDef
stalkingManticore =
  (enemy "06146" "Stalking Manticore" TheSearchForKadath 1)
    { cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [Creature, Monster, Elite]
    , cdVictoryPoints = Just 1
    }

hordeOfNight :: CardDef
hordeOfNight =
  (enemy "06147" "Horde of Night" TheSearchForKadath 1)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Elite]
    , cdVictoryPoints = Just 1
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Swarming (Static 1)]
    }

beingsOfIb :: CardDef
beingsOfIb =
  (enemy "06148" "Beings of Ib" TheSearchForKadath 1)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Elite]
    , cdVictoryPoints = Just 1
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter, Keyword.Swarming (PerPlayer 1)]
    }

priestOfAThousandMasks :: CardDef
priestOfAThousandMasks =
  (enemy "06149" "Priest of a Thousand Masks" TheSearchForKadath 3)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

tenebrousNightgaunt :: CardDef
tenebrousNightgaunt =
  (enemy "06150" "Tenebrous Nightgaunt" TheSearchForKadath 2)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Nightgaunt]
    , cdKeywords = singleton Keyword.Hunter
    }

packOfVooniths :: CardDef
packOfVooniths =
  (enemy "06151" "Pack of Vooniths" TheSearchForKadath 2)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = singleton (Keyword.Swarming (Static 1))
    }

nightriders :: CardDef
nightriders =
  (enemy "06152" "Nightriders" TheSearchForKadath 2)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = singleton (Keyword.Swarming (Static 1))
    }

theUnnamable :: CardDef
theUnnamable =
  unique
    $ doubleSided "06169"
    $ (enemy "06169b" ("The Unnamable" <:> "The Ultimate Abomination") AThousandShapesOfHorror 1)
      { cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      }

moonLizard :: CardDef
moonLizard =
  (enemy "06226" "Moon Lizard" DarkSideOfTheMoon 1)
    { cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 2
    }

moonboundByakhee :: CardDef
moonboundByakhee =
  (enemy "06227" "Moonbound Byakhee" DarkSideOfTheMoon 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

catsFromSaturn :: CardDef
catsFromSaturn =
  (enemy "06228" "Cats from Saturn" DarkSideOfTheMoon 3)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Swarming (Static 0)]
    }

moonBeast :: CardDef
moonBeast =
  (enemy "06229" "Moon-Beast" DarkSideOfTheMoon 2)
    { cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Servitor]
    , cdKeywords = singleton Keyword.Retaliate
    , cdVictoryPoints = Just 1
    }

gugSentinel :: CardDef
gugSentinel =
  (enemy "06267" "Gug Sentinel" PointOfNoReturn 1)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Gug]
    , cdVictoryPoints = Just 1
    }

slitheringDhole :: CardDef
slitheringDhole =
  (enemy "06271" "Slithering Dhole" TerrorOfTheVale 1)
    { cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Dhole, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

pitchSpider :: CardDef
pitchSpider =
  (enemy "06273" "Pitch Spider" TerrorOfTheVale 2)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Spider]
    , cdKeywords = setFromList [Keyword.Swarming (Static 0)]
    }

unboundBeast :: CardDef
unboundBeast =
  (weakness "06283" "Unbound Beast")
    { cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdRevelation = IsRevelation
    }

nyarlathotepTheCrawlingChaos :: CardDef
nyarlathotepTheCrawlingChaos =
  unique
    $ (enemy "06306" ("Nyarlathotep" <:> "The Crawling Chaos") WhereTheGodsDwell 1)
      { cdHealth = health 5
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepTheFacelessWhisperer :: CardDef
nyarlathotepTheFacelessWhisperer =
  unique
    $ (enemy "06307" ("Nyarlathotep" <:> "The Faceless Whisperer") WhereTheGodsDwell 1)
      { cdHealth = health 3
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepMessengerOfTheOuterGods :: CardDef
nyarlathotepMessengerOfTheOuterGods =
  unique
    $ (enemy "06308" ("Nyarlathotep" <:> "Messenger of the Outer Gods") WhereTheGodsDwell 1)
      { cdHealth = health 4
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Alert]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepGodOfAThousandForms :: CardDef
nyarlathotepGodOfAThousandForms =
  unique
    $ (enemy "06309" ("Nyarlathotep" <:> "God of a Thousand Forms") WhereTheGodsDwell 1)
      { cdHealth = health 6
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepStalkerAmongTheStars :: CardDef
nyarlathotepStalkerAmongTheStars =
  unique
    $ (enemy "06310" ("Nyarlathotep" <:> "Stalker Among the Stars") WhereTheGodsDwell 1)
      { cdHealth = health 7
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Massive]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

highPriestNotToBeDescribed :: CardDef
highPriestNotToBeDescribed =
  unique
    $ (enemy "06311" ("High Priest Not to Be Described" <:> "Agent of the Other Gods") WhereTheGodsDwell 1)
      { cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Monster, Cultist, Avatar, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

dholeOfTheWastes :: CardDef
dholeOfTheWastes =
  (enemy "06312" "Dhole of the Wastes" WhereTheGodsDwell 1)
    { cdHealth = health 6
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

liarWithNoFace :: CardDef
liarWithNoFace =
  (enemy "06313" "Liar with No Face" WhereTheGodsDwell 3)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Cultist, Servitor]
    , cdKeywords = singleton Keyword.Hunter
    }

atlachNacha :: CardDef
atlachNacha =
  unique
    $ doubleSided "06346b"
    $ (enemy "06346" ("Atlach-Nacha" <:> "The Spider God") WeaverOfTheCosmos 1)
      { cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [AncientOne, Spider, Elite]
      , cdKeywords = singleton Keyword.Massive
      , cdVictoryPoints = Just 1
      }

legsOfAtlachNacha_347 :: CardDef
legsOfAtlachNacha_347 =
  (enemy "06347" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

legsOfAtlachNacha_348 :: CardDef
legsOfAtlachNacha_348 =
  (enemy "06348" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

legsOfAtlachNacha_349 :: CardDef
legsOfAtlachNacha_349 =
  (enemy "06349" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

legsOfAtlachNacha_350 :: CardDef
legsOfAtlachNacha_350 =
  (enemy "06350" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

webSpinner :: CardDef
webSpinner =
  (enemy "06351" "Web-Spinner" WeaverOfTheCosmos 3)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Spider]
    , cdKeywords = singleton Keyword.Aloof
    }
