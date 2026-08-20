module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

angryMob :: CardDef
angryMob =
  doubleSided "07062"
    $ (enemy "07062b" "Angry Mob" TheVanishingOfElinaHarper 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

barnabasMarshTheChangeIsUponHim :: CardDef
barnabasMarshTheChangeIsUponHim =
  unique
    $ (enemy "07079" ("Barnabas Marsh" <:> "The Change Is upon Him") TheVanishingOfElinaHarper 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

brianBurnhamWantsOut :: CardDef
brianBurnhamWantsOut =
  unique
    $ (enemy "07078" ("Brian Burnham" <:> "Wants Out") TheVanishingOfElinaHarper 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 5
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

joyceLittleBookshopOwner :: CardDef
joyceLittleBookshopOwner =
  unique
    $ (enemy "07080" ("Joyce Little" <:> "Bookshop Owner") TheVanishingOfElinaHarper 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 5
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

otheraGilmanProprietessOfTheHotel :: CardDef
otheraGilmanProprietessOfTheHotel =
  unique
    $ (enemy "07081" ("Othera Gilman" <:> "Proprietess of the Hotel") TheVanishingOfElinaHarper 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

robertFriendlyDisgruntledDockworker :: CardDef
robertFriendlyDisgruntledDockworker =
  unique
    $ (enemy "07076" ("Robert Friendly" <:> "Disgruntled Dockerworker") TheVanishingOfElinaHarper 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 1
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

zadokAllenDrunkAndDisorderly :: CardDef
zadokAllenDrunkAndDisorderly =
  unique
    $ (enemy "07077" ("Zadok Allen" <:> "Drunk and Disorderly") TheVanishingOfElinaHarper 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }
