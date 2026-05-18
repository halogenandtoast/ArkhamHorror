module Arkham.Enemy.CardDefs.NightOfTheZealot where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

mobEnforcer :: CardDef
mobEnforcer =
  (basicWeakness "01101" "Mob Enforcer")
    { cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdAlternateCardCodes = ["01601"]
    }

silverTwilightAcolyte :: CardDef
silverTwilightAcolyte =
  (basicWeakness "01102" "Silver Twilight Acolyte")
    { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdAlternateCardCodes = ["01602"]
    }

stubbornDetective :: CardDef
stubbornDetective =
  (basicWeakness "01103" "Stubborn Detective")
    { cdCardTraits = setFromList [Humanoid, Detective]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdAlternateCardCodes = ["01603"]
    }

ghoulPriest :: CardDef
ghoulPriest =
  (enemy "01116" "Ghoul Priest" TheGathering 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }

fleshEater :: CardDef
fleshEater =
  (enemy "01118" "Flesh-Eater" TheGathering 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdVictoryPoints = Just 1
    }

icyGhoul :: CardDef
icyGhoul =
  (enemy "01119" "Icy Ghoul" TheGathering 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdVictoryPoints = Just 1
    }

theMaskedHunter :: CardDef
theMaskedHunter =
  unique
    $ doubleSided "01121a"
    $ ( enemy
          "01121b"
          ("The Masked Hunter" <:> "Silently Stalking")
          TheMidnightMasks
          1
      )
      { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 2
      }

wolfManDrew :: CardDef
wolfManDrew =
  unique
    $ (enemy "01137" ("\"Wolf-Man\" Drew" <:> "The Cannibal") CultOfUmordhoth 1)
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

hermanCollins :: CardDef
hermanCollins =
  unique
    $ (enemy "01138" ("Herman Collins" <:> "The Undertaker") CultOfUmordhoth 1)
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

peterWarren :: CardDef
peterWarren =
  unique
    $ (enemy "01139" ("Peter Warren" <:> "The Occult Professor") CultOfUmordhoth 1)
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

victoriaDevereux :: CardDef
victoriaDevereux =
  unique
    $ (enemy "01140" ("Victoria Devereux" <:> "The Collector") CultOfUmordhoth 1)
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

ruthTurner :: CardDef
ruthTurner =
  unique
    $ (enemy "01141" ("Ruth Turner" <:> "The Mortician") CultOfUmordhoth 1)
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

umordhoth :: CardDef
umordhoth =
  unique
    $ (enemy "01157" ("Umôrdhoth" <:> "The Devourer Below") TheDevourerBelow 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      }

swarmOfRats :: CardDef
swarmOfRats =
  (enemy "01159" "Swarm of Rats" Rats 3)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

ghoulMinion :: CardDef
ghoulMinion =
  (enemy "01160" "Ghoul Minion" Ghouls 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }

ravenousGhoul :: CardDef
ravenousGhoul =
  (enemy "01161" "Ravenous Ghoul" Ghouls 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }

acolyte :: CardDef
acolyte =
  (enemy "01169" "Acolyte" DarkCult 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

wizardOfTheOrder :: CardDef
wizardOfTheOrder =
  (enemy "01170" "Wizard of the Order" DarkCult 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

huntingNightgaunt :: CardDef
huntingNightgaunt =
  (enemy "01172" "Hunting Nightgaunt" Nightgaunts 2)
    { cdCardTraits = setFromList [Monster, Nightgaunt]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

screechingByakhee :: CardDef
screechingByakhee =
  (enemy "01175" "Screeching Byakhee" AgentsOfHastur 2)
    { cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

yithianObserver :: CardDef
yithianObserver =
  (enemy "01177" "Yithian Observer" AgentsOfYogSothoth 2)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdVictoryPoints = Just 1
    }

relentlessDarkYoung :: CardDef
relentlessDarkYoung =
  (enemy "01179" "Relentless Dark Young" AgentsOfShubNiggurath 1)
    { cdCardTraits = setFromList [Monster, DarkYoung]
    , cdVictoryPoints = Just 1
    }

goatSpawn :: CardDef
goatSpawn =
  (enemy "01180" "Goat Spawn" AgentsOfShubNiggurath 3)
    { cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

youngDeepOne :: CardDef
youngDeepOne =
  (enemy "01181" "Young Deep One" AgentsOfCthulhu 2)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
