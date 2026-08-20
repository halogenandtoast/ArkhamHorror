module Arkham.Enemy.CardDefs.TheMidwinterGala where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

abhorrentMoonBeast :: CardDef
abhorrentMoonBeast =
  (enemy "71048" "Abhorrent Moon-Beast" TheMidwinterGala 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Dreamlands, Servitor]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

caldwellPhilipsCompelledByDreams :: CardDef
caldwellPhilipsCompelledByDreams =
  unique
    $ doubleSided "71022"
    $ (enemy "71022b" ("Caldwell Philips" <:> "Compelled by Dreams") TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

carlSanfordIntimidatingPresence :: CardDef
carlSanfordIntimidatingPresence =
  unique
    $ doubleSided "71034"
    $ (enemy "71034b" ("Carl Sanford" <:> "Intimidating Presence") TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

declanPearce :: CardDef
declanPearce =
  unique
    $ (enemy "71051" "Declan Pearce" TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, LanternClub, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

enragedGug :: CardDef
enragedGug =
  (enemy "71047" "Enraged Gug" TheMidwinterGala 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Dreamlands, Gug]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

johnnyValoneHereToCollect :: CardDef
johnnyValoneHereToCollect =
  unique
    $ doubleSided "71028"
    $ (enemy "71028b" ("Johnny Valone" <:> "Here to Collect") TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

lanternClubMember :: CardDef
lanternClubMember =
  (enemy "71053" "Lantern Club Member" TheMidwinterGala 4)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, LanternClub]
    }

rookieCop :: CardDef
rookieCop =
  (enemy "71020" "Rookie Cop" TheMidwinterGala 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Police, Rival]
    , cdKeywords = setFromList [Keyword.Surge, Keyword.Aloof, Keyword.Hunter]
    }

savageShantak :: CardDef
savageShantak =
  (enemy "71049" "Savage Shantak" TheMidwinterGala 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Dreamlands, Shantak]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theBloodlessMan :: CardDef
theBloodlessMan =
  unique
    $ doubleSided "71045b"
    $ (enemy "71045" "The Bloodless Man" TheMidwinterGala 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Humanoid, LanternClub, Elite]
      , cdKeywords =
          setFromList
            [ Keyword.Aloof
            , Keyword.Patrol (LocationWithAsset $ AssetWithTrait Guest)
            ]
      }

theBloodlessManUnleashed :: CardDef
theBloodlessManUnleashed =
  unique
    $ doubleSided "71045"
    $ (enemy "71045b" "The Bloodless Man" TheMidwinterGala 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      , cdVictoryPoints = Just 1
      }

valeriyaAntonovaDontMessWithHer :: CardDef
valeriyaAntonovaDontMessWithHer =
  unique
    $ doubleSided "71016"
    $ (enemy "71016b" ("Valeriya Antonova" <:> "Don't Mess With Her") TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

williamBainDefiantToTheLast :: CardDef
williamBainDefiantToTheLast =
  unique
    $ doubleSided "71040"
    $ (enemy "71040b" ("William Bain" <:> "Defiant to the Last") TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }
