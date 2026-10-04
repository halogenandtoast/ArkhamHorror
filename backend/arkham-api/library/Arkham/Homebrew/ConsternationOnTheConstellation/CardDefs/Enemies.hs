{- | Consternation on the Constellation's enemies.

Luther Marsh and the Colossal Servant are the two faces of one card: whichever
branch the scenario takes spawns the side that belongs to it, so only one of
them is ever in play.

Hand of the Strangler is the back of a Crate of Goods -- a relic that fights as
an enemy. It prints no fight, health or evade at all (it cannot be defeated or
evaded), which is why it leaves 'cdFight', 'cdHealth' and 'cdEvade' unset.
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.ConsternationOnTheConstellation.Sets qualified as Set
import Arkham.Keyword qualified as Keyword

-- | The back of Crate of Goods @:011@.
handOfTheStrangler :: CardDef
handOfTheStrangler =
  ( enemy
      ":consternation-on-the-constellation:011b"
      "Hand of the Strangler"
      Set.ConsternationOnTheConstellation
      1
  )
    { cdCardTraits = setFromList [Relic, Elite]
    , cdHealthDamage = healthDamage 1
    , cdKeywords = singleton Keyword.Hunter
    , cdUnique = True
    , cdOtherSide = Just ":consternation-on-the-constellation:011"
    }

-- | Spawned at the Bridge by agenda 2b, with the Tablet of Dagon attached.
lutherMarsh :: CardDef
lutherMarsh =
  ( enemy
      ":consternation-on-the-constellation:027"
      ("Luther Marsh" <:> "Empowered and Enthralled")
      Set.ConsternationOnTheConstellation
      1
  )
    { cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
    , cdFight = fight 4
    , cdHealth = healthPerInvestigator 5
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    , cdOtherSide = Just ":consternation-on-the-constellation:027b"
    }

-- | Spawned at Open Water by act 2b. The other face of Luther Marsh.
colossalServant :: CardDef
colossalServant =
  ( enemy
      ":consternation-on-the-constellation:027b"
      ("Colossal Servant" <:> "Monster from the Depths")
      Set.ConsternationOnTheConstellation
      1
  )
    { cdCardTraits = setFromList [Monster, DeepOne, Elite]
    , cdFight = fight 5
    , cdHealth = healthPerInvestigator 7
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Alert]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    , cdOtherSide = Just ":consternation-on-the-constellation:027"
    }

cultistOfTheDeep :: CardDef
cultistOfTheDeep =
  ( enemy
      ":consternation-on-the-constellation:028"
      "Cultist of the Deep"
      Set.ConsternationOnTheConstellation
      3
  )
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdFight = fight 3
    , cdHealth = health 3
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    }

-- | One copy is spawned at Cargo Room during setup; the other starts in the deck.
orderEnforcer :: CardDef
orderEnforcer =
  ( enemy
      ":consternation-on-the-constellation:029"
      "Order Enforcer"
      Set.ConsternationOnTheConstellation
      2
  )
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 2
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

seaSinger :: CardDef
seaSinger =
  (enemy ":consternation-on-the-constellation:030" "Sea Singer" Set.ConsternationOnTheConstellation 2)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdFight = fight 4
    , cdHealth = health 2
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdKeywords = singleton Keyword.Aloof
    }

-- * Deep Ones

dagonWarrior :: CardDef
dagonWarrior =
  (enemy ":consternation-on-the-constellation:039" "Dagon Warrior" Set.DeepOnes 2)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdFight = fight 4
    , cdHealth = health 4
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

destroyerFromTheDepths :: CardDef
destroyerFromTheDepths =
  (enemy ":consternation-on-the-constellation:040" "Destroyer from the Depths" Set.DeepOnes 3)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    }

wakeTitan :: CardDef
wakeTitan =
  (enemy ":consternation-on-the-constellation:041" "Wake Titan" Set.DeepOnes 1)
    { cdCardTraits = setFromList [Monster, DeepOne, Elite]
    , cdFight = fight 5
    , cdHealth = health 5
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }
