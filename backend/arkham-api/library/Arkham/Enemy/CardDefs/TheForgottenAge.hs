module Arkham.Enemy.CardDefs.TheForgottenAge where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

serpentsOfYig :: CardDef
serpentsOfYig =
  (weakness "04014" "Serpents of Yig")
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    , cdRevelation = IsRevelation
    }

ichtaca :: CardDef
ichtaca =
  unique
    $ (enemy "04052" ("Ichtaca" <:> "Keeper of the Eztli") TheUntamedWilds 1)
      { cdCardTraits = setFromList [Humanoid, Eztli, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

harbingerOfValusia :: CardDef
harbingerOfValusia =
  unique
    $ ( enemy
          "04062"
          ("Harbinger of Valusia" <:> "The Sleeper Awakens")
          TheDoomOfEztli
          1
      )
      { cdCardTraits = setFromList [Humanoid, Serpent, Monster, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
      , cdVengeancePoints = Just 5
      }

pitViper :: CardDef
pitViper =
  (enemy "04078" "Pit Viper" Serpents 3)
    { cdCardTraits = setFromList [Creature, Serpent]
    , cdVengeancePoints = Just 1
    }

boaConstrictor :: CardDef
boaConstrictor =
  (enemy "04079" "Boa Constrictor" Serpents 1)
    { cdCardTraits = setFromList [Creature, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    , cdVengeancePoints = Just 2
    }

broodOfYig :: CardDef
broodOfYig =
  (enemy "04083" "Brood of Yig" AgentsOfYig 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    }

serpentFromYoth :: CardDef
serpentFromYoth =
  (enemy "04084" "Serpent from Yoth" AgentsOfYig 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdVictoryPoints = Just 1
    }

eztliGuardian :: CardDef
eztliGuardian =
  (enemy "04086" "Eztli Guardian" GuardiansOfTime 2)
    { cdCardTraits = setFromList [Humanoid, Eztli]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof]
    }

brotherhoodCultist :: CardDef
brotherhoodCultist =
  (enemy "04095" "Brotherhood Cultist" PnakoticBrotherhood 2)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Hunter
    }

fangOfYig :: CardDef
fangOfYig =
  (enemy "04098" "Fang of Yig" YigsVenom 2)
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = singleton Keyword.Retaliate
    }

harlanEarnstoneCrazedByTheCurse :: CardDef
harlanEarnstoneCrazedByTheCurse =
  unique
    $ doubleSided "04122"
    $ (enemy "04122b" ("Harlan Earnstone" <:> "Crazed by the Curse") ThreadsOfFate 1)
      { cdCardTraits = setFromList [Humanoid, Cursed, Elite]
      , cdVictoryPoints = Just 1
      }

henryDeveauAlejandrosKidnapper :: CardDef
henryDeveauAlejandrosKidnapper =
  unique
    $ doubleSided "04130"
    $ (enemy "04130b" ("Henry Deveau" <:> "Alejandro's Kidnapper") ThreadsOfFate 1)
      { cdCardTraits = setFromList [Humanoid, Conspirator, Elite]
      , cdVictoryPoints = Just 1
      , cdKeywords = singleton Keyword.Retaliate
      }

mariaDeSilvaKnowsMoreThanSheLetsOn :: CardDef
mariaDeSilvaKnowsMoreThanSheLetsOn =
  unique
    $ doubleSided "04137"
    $ (enemy "04137b" ("Maria DeSilva" <:> "Knows More Than She Lets On") ThreadsOfFate 1)
      { cdCardTraits = setFromList [Humanoid, Conspirator, Elite]
      , cdVictoryPoints = Just 1
      , cdKeywords = singleton Keyword.Retaliate
      }

padmaAmrita :: CardDef
padmaAmrita =
  unique
    $ (enemy "04186" ("Padma Amrita" <:> "Cold-Blooded Charmer") TheBoundaryBeyond 1)
      { cdCardTraits = setFromList [Humanoid, Serpent, Servitor, Elite]
      , cdVictoryPoints = Just 2
      , cdVengeancePoints = Just 2
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate, Keyword.Hunter]
      }

serpentOfTenochtitlan :: CardDef
serpentOfTenochtitlan =
  (enemy "04187" "Serpent of Tenochtitlán" TheBoundaryBeyond 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdVictoryPoints = Just 1
    , cdVengeancePoints = Just 1
    }

handOfTheBrotherhood :: CardDef
handOfTheBrotherhood =
  (enemy "04188" "Hand of the Brotherhood" TheBoundaryBeyond 2)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

theWingedSerpent :: CardDef
theWingedSerpent =
  doubleSided "04209"
    $ (enemy "04209b" ("The Winged Serpent" <:> "The Wrath of Yig") PillarsOfJudgement 1)
      { cdCardTraits = setFromList [Monster, Serpent, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Massive]
      }

apexStrangleweed :: CardDef
apexStrangleweed =
  (enemy "04219" "Apex Strangleweed" PillarsOfJudgement 2)
    { cdCardTraits = setFromList [Creature, Flora]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

basilisk :: CardDef
basilisk =
  (enemy "04220" "Basilisk" PillarsOfJudgement 2)
    { cdCardTraits = setFromList [Monster, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    , cdVengeancePoints = Just 2
    }

keeperOfTheGreatLibrary :: CardDef
keeperOfTheGreatLibrary =
  (enemy "04257" "Keeper of the Great Library" TheCityOfArchives 2)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof]
    }

scientistOfYith :: CardDef
scientistOfYith =
  (enemy "04258" "Scientist of Yith" TheCityOfArchives 2)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = singleton Keyword.Aloof
    }

scholarFromYith :: CardDef
scholarFromYith =
  (enemy "04259" "Scholar from Yith" TheCityOfArchives 3)
    { cdCardTraits = setFromList [Monster, Yithian]
    }

yig :: CardDef
yig =
  unique
    $ (enemy "04296" ("Yig" <:> "The Father of Serpents") TheDepthsOfYoth 1)
      { cdCardTraits = setFromList [AncientOne, Serpent, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      , cdVictoryPoints = Just 5
      }

pitWarden :: CardDef
pitWarden =
  (enemy "04297" "Pit Warden" TheDepthsOfYoth 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdVengeancePoints = Just 1
    , cdKeywords = singleton Keyword.Hunter
    }

eaterOfTheDepths :: CardDef
eaterOfTheDepths =
  (enemy "04298" "Eater of the Depths" TheDepthsOfYoth 1)
    { cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 2
    }

ichtacaScionOfYig :: CardDef
ichtacaScionOfYig =
  unique
    $ doubleSided "04325b"
    $ (enemy "04325" ("Ichtaca" <:> "Scion of Yig") ShatteredAeons 1)
      { cdCardTraits = setFromList [Humanoid, Monster, Serpent, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

alejandroVela :: CardDef
alejandroVela =
  unique
    $ doubleSided "04326b"
    $ (enemy "04326" ("Alejandro Vela" <:> "Or, Is He?") ShatteredAeons 1)
      { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      }

formlessSpawn :: CardDef
formlessSpawn =
  (enemy "04337" ("Formless Spawn" <:> "From the Abyss") ShatteredAeons 1)
    { cdCardTraits = setFromList [Monster, Abomination, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 2
    }

temporalDevourer :: CardDef
temporalDevourer =
  (enemy "04338" "Temporal Devourer" ShatteredAeons 2)
    { cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = singleton Keyword.Hunter
    }
