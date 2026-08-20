module Arkham.Enemy.CardDefs.BrethrenOfAsh.QueenOfAsh where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

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
