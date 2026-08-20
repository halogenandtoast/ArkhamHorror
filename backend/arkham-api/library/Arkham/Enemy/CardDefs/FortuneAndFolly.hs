module Arkham.Enemy.CardDefs.FortuneAndFolly where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

abarranArrigorriagakoaAbarranUnleashed :: CardDef
abarranArrigorriagakoaAbarranUnleashed =
  doubleSided "88034a"
    $ (enemy "88034b" ("Abarran Arrigorriagakoa" <:> "Abarran Unleashe") FortuneAndFolly 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Humanoid, Casino, Coterie, Elite]
      , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "ace")]
      , cdUnique = True
      , cdVictoryPoints = Just 1
      }

abarranArrigorriagakoaTheManWithTheRubyRing :: CardDef
abarranArrigorriagakoaTheManWithTheRubyRing =
  doubleSided "88034b"
    $ (enemy "88034a" ("Abarran Arrigorriagakoa" <:> "The Man with the RUby Ring") FortuneAndFolly 1)
      { cdCardTraits = setFromList [Humanoid, Casino, Coterie, Elite]
      , cdKeywords =
          setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "abarranNext"))]
      , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "ace")]
      , cdUnique = True
      }

casinoGuardA :: CardDef
casinoGuardA =
  (enemy "88035a" "Casino Guard" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "casinoGuardANext"))]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "queen")]
    }

casinoGuardB :: CardDef
casinoGuardB =
  (enemy "88035b" "Casino Guard" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "casinoGuardBNext"))]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "jack")]
    }

casinoGuardC :: CardDef
casinoGuardC =
  (enemy "88035c" "Casino Guard" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "casinoGuardCNext"))]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "ten")]
    }

dimensionalDisplacerA :: CardDef
dimensionalDisplacerA =
  (enemy "88050a" "Dimensional Displacer" PlanInShambles 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "six")]
    }

dimensionalDisplacerB :: CardDef
dimensionalDisplacerB =
  (enemy "88050b" "Dimensional Displacer" PlanInShambles 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "six")]
    }

dimensionalDuplicatorA :: CardDef
dimensionalDuplicatorA =
  (enemy "88049a" "Dimensional Duplicator" PlanInShambles 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "six")]
    }

dimensionalDuplicatorB :: CardDef
dimensionalDuplicatorB =
  (enemy "88049b" "Dimensional Duplicator" PlanInShambles 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "six")]
    }

dimensionalShamblerHunterFromBeyond :: CardDef
dimensionalShamblerHunterFromBeyond =
  (enemy "88048" ("Dimensional Shambler" <:> "Hunter from Beyond") PlanInShambles 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 6
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Extradimensional, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "ace")]
    , cdVictoryPoints = Just 1
    }

fortunesDaggerA :: CardDef
fortunesDaggerA =
  (enemy "88047a" "Fortune's Dagger" FortunesChosen 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Casino, Cultist]
    , cdKeywords =
        setFromList
          [Keyword.Patrol (LocationWithModifier (ScenarioModifier "fortunesDaggerANext")), Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "king")]
    }

fortunesDaggerB :: CardDef
fortunesDaggerB =
  (enemy "88047b" "Fortune's Dagger" FortunesChosen 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Casino, Cultist]
    , cdKeywords =
        setFromList
          [Keyword.Patrol (LocationWithModifier (ScenarioModifier "fortunesDaggerBNext")), Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "king")]
    }

fortunesShieldA :: CardDef
fortunesShieldA =
  (enemy "88046a" "Fortune's Shield" FortunesChosen 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Casino, Cultist]
    , cdKeywords =
        setFromList
          [Keyword.Patrol (LocationWithModifier (ScenarioModifier "fortunesShieldANext")), Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "king")]
    }

fortunesShieldB :: CardDef
fortunesShieldB =
  (enemy "88046b" "Fortune's Shield" FortunesChosen 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Casino, Cultist]
    , cdKeywords =
        setFromList
          [Keyword.Patrol (LocationWithModifier (ScenarioModifier "fortunesShieldBNext")), Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "king")]
    }

houseDealerA :: CardDef
houseDealerA =
  (enemy "88036a" "House Dealer" FortuneAndFolly 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "houseDealerANext"))]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "nine")]
    }

houseDealerB :: CardDef
houseDealerB =
  (enemy "88036b" "House Dealer" FortuneAndFolly 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "houseDealerBNext"))]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "nine")]
    }

securityPatrolA :: CardDef
securityPatrolA =
  (enemy "88037a" "Security Patrol" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "securityPatrolANext"))]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "queen")]
    }

securityPatrolB :: CardDef
securityPatrolB =
  (enemy "88037b" "Security Patrol" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "securityPatrolBNext"))]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "jack")]
    }

securityPatrolC :: CardDef
securityPatrolC =
  (enemy "88037c" "Security Patrol" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "securityPatrolCNext"))]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "ten")]
    }
