module Arkham.Enemy.CardDefs.BrethrenOfAsh.SmokeAndMirrors where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

abigailForemanWaryLibrarian :: CardDef
abigailForemanWaryLibrarian =
  unique
    $ (enemy "12143" ("Abigail Foreman" <:> "Wary Librarian") PeopleOfArkham 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Miskatonic, Elite]
      , cdKeywords = setFromList [Keyword.Aloof]
      , cdVictoryPoints = Just 1
      }

corneliaAkelyExhaustedSupervisor :: CardDef
corneliaAkelyExhaustedSupervisor =
  unique
    $ (enemy "12140" ("Cornelia Akely" <:> "Exhausted Supervisor") PeopleOfArkham 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Worker, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      , cdVictoryPoints = Just 1
      }

davidRenfieldDisillusionedEschatologist :: CardDef
davidRenfieldDisillusionedEschatologist =
  unique
    $ (enemy "12139" ("David Renfield" <:> "Disillusioned Eschatologist") PeopleOfArkham 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 1
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, SilverTwilight, Elite]
      , cdKeywords = setFromList [Keyword.Aloof]
      , cdVictoryPoints = Just 1
      }

margaretLiuBeguilingLoungeSinger :: CardDef
margaretLiuBeguilingLoungeSinger =
  unique
    $ (enemy "12144" ("Margaret Liu" <:> "Beguiling Lounge Singer") PeopleOfArkham 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 1
      , cdEvade = evade 5
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Socialite, Elite]
      , cdKeywords = setFromList [Keyword.Aloof]
      , cdVictoryPoints = Just 1
      }

naomiOBannionRunsThisTown :: CardDef
naomiOBannionRunsThisTown =
  unique
    $ (enemy "12141" ("Naomi O'Bannion" <:> "Runs This Town") PeopleOfArkham 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Syndicate, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

rogueGangster :: CardDef
rogueGangster =
  (enemy "12164" "Rogue Gangster" GangsOfArkham 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Syndicate]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

servantOfFlameOnTheRun :: CardDef
servantOfFlameOnTheRun =
  unique
    $ (enemy "12138" ("Servant of Flame" <:> "On the Run") SmokeAndMirrors 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

sgtEarlMonroeDirtyCop :: CardDef
sgtEarlMonroeDirtyCop =
  unique
    $ (enemy "12142" ("Sgt. Earl Monroe" <:> "Dirty Cop") PeopleOfArkham 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Police, Elite]
      , cdKeywords = setFromList [Keyword.Elusive]
      , cdVictoryPoints = Just 1
      }

whippoorwill2 :: CardDef
whippoorwill2 =
  (enemy "12166" "Whippoorwill" Whippoorwills2 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }
