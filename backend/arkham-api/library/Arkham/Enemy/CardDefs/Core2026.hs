module Arkham.Enemy.CardDefs.Core2026 where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

blackChamberOperative :: CardDef
blackChamberOperative =
  (weakness "12009" "Black Chamber Operative")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theNamelessLurker :: CardDef
theNamelessLurker =
  unique
    $ (basicWeakness "12099" "The Nameless Lurker")
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 1
      , cdCardTraits = setFromList [Humanoid, Monster]
      , cdKeywords = setFromList [Keyword.Aloof]
      }

servantOfFlameRagingFury :: CardDef
servantOfFlameRagingFury =
  unique
    $ (enemy "12114" ("Servant of Flame" <:> "Raging Fury") SpreadingFlames 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

cantorOfFlame :: CardDef
cantorOfFlame =
  (enemy "12121" "Cantor of Flame" AshenPilgrims 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

hellhound :: CardDef
hellhound =
  (enemy "12122" "Hellhound" AshenPilgrims 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

bystander :: CardDef
bystander =
  (enemy "12123" "Bystander" Bystanders 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Civilian]
    , cdKeywords = setFromList [Keyword.Doomed]
    }

mutatedExperiment :: CardDef
mutatedExperiment =
  (enemy "12132" "Mutated Experiment" MadScience 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Mutated]
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

batHorror :: CardDef
batHorror =
  (enemy "12162" "Bat Horror" FlyingTerrors 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
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

queensKnight :: CardDef
queensKnight =
  unique
    $ (enemy "12177" "Queen`s Knight" QueenOfAsh 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }

heraldOfFlame :: CardDef
heraldOfFlame =
  unique
    $ (enemy "12178" "Herald Of Flame" QueenOfAsh 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 5
      , cdCardTraits = setFromList [Monster]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }

elokossFaintEmbers :: CardDef
elokossFaintEmbers =
  doubleSided "12179b"
    $ (enemy "12179" ("Elokoss" <:> "Faint Embers") QueenOfAsh 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdHealth = health 5
      , cdCardTraits = setFromList [AncientOne, Flora, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 5
      }

elokossMotherOfFlame :: CardDef
elokossMotherOfFlame =
  doubleSided "12179"
    $ (enemy "12179b" ("Elokoss" <:> "Mother of Flame") QueenOfAsh 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdHealth = health 5
      , cdCardTraits = setFromList [AncientOne, Flora, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 5
      }

servantOfFlameAWillingSacrifice :: CardDef
servantOfFlameAWillingSacrifice =
  unique
    $ (enemy "12180" ("Servant of Flame" <:> "A Willing Sacrifice") QueenOfAsh 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

zealot :: CardDef
zealot =
  (enemy "12188" "Zealot" Cultists 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

darkMagician :: CardDef
darkMagician =
  (enemy "12189" "Dark Magician" Cultists 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }
