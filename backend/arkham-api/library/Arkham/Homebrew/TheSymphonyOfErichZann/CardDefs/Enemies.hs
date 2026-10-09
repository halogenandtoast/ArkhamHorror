{- | The Symphony of Erich Zann's enemies.

The four named @Musician@ enemies are each locked behind one instrument trait:
while no treachery of that instrument is in play they can be neither parleyed
with nor damaged. Each flips to a story card when its parley succeeds.
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.TheSymphonyOfErichZann.Sets qualified as Set
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits hiding (pattern Piano)
import Arkham.Keyword qualified as Keyword

-- | The back of act 1. Only enters play when that act advances.
augusteGaudinConductorOfTheVoid :: CardDef
augusteGaudinConductorOfTheVoid =
  ( enemy
      ":the-symphony-of-erich-zann:005b"
      ("Auguste Gaudin" <:> "Conductor of the Void")
      Set.TheSymphonyOfErichZann
      1
  )
    { cdCardTraits = setFromList [Humanoid, Musician, Elite]
    , cdFight = fight 3
    , cdHealth = healthPerInvestigator 2
    , cdEvade = evade 2
    , cdUnique = True
    , cdOtherSide = Just ":the-symphony-of-erich-zann:005"
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    }

arnoldWalker :: CardDef
arnoldWalker =
  ( enemy
      ":the-symphony-of-erich-zann:020"
      ("Arnold Walker" <:> "Crazed Trumpeter")
      Set.TheSymphonyOfErichZann
      1
  )
    { cdCardTraits = setFromList [Humanoid, Musician, Elite]
    , cdFight = fight 5
    , cdHealth = health 3
    , cdEvade = evade 2
    , cdKeywords = singleton Keyword.Aloof
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdOtherSide = Just ":the-symphony-of-erich-zann:020b"
    , cdSanityDamage = sanityDamage 2
    }

isabelLaFratta :: CardDef
isabelLaFratta =
  ( enemy
      ":the-symphony-of-erich-zann:021"
      ("Isabel La Fratta" <:> "Delirious Pianist")
      Set.TheSymphonyOfErichZann
      1
  )
    { cdCardTraits = setFromList [Humanoid, Musician, Elite]
    , cdFight = fight 2
    , cdHealth = health 6
    , cdEvade = evade 2
    , cdKeywords = singleton Keyword.Aloof
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdOtherSide = Just ":the-symphony-of-erich-zann:021b"
    , cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    }

nicolePage :: CardDef
nicolePage =
  ( enemy
      ":the-symphony-of-erich-zann:022"
      ("Nicole Page" <:> "Frenzied Violinist")
      Set.TheSymphonyOfErichZann
      1
  )
    { cdCardTraits = setFromList [Humanoid, Musician, Elite]
    , cdFight = fight 3
    , cdHealth = health 4
    , cdEvade = evade 4
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Alert]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdOtherSide = Just ":the-symphony-of-erich-zann:022b"
    , cdHealthDamage = healthDamage 2
    }

songYin :: CardDef
songYin =
  ( enemy
      ":the-symphony-of-erich-zann:023"
      ("Song Yin" <:> "Erratic Percussionist")
      Set.TheSymphonyOfErichZann
      1
  )
    { cdCardTraits = setFromList [Humanoid, Musician, Elite]
    , cdFight = fight 4
    , cdHealth = health 4
    , cdEvade = evade 3
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdOtherSide = Just ":the-symphony-of-erich-zann:023b"
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    }

earsOfTheVoid :: CardDef
earsOfTheVoid =
  (enemy ":the-symphony-of-erich-zann:031" "Ears of the Void" Set.TheSymphonyOfErichZann 2)
    { cdCardTraits = singleton Extradimensional
    , cdFight = fight 2
    , cdHealth = health 2
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 2
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

macabreDancers :: CardDef
macabreDancers =
  (enemy ":the-symphony-of-erich-zann:036" "Macabre Dancers" Set.TheSymphonyOfErichZann 2)
    { cdCardTraits = singleton Geist
    , cdFight = fight 3
    , cdHealth = health 3
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    }

{- | Three Dancing Rats, one per card: same spawn and Hunter, but each turns
aloof against a different instrument, and each has its own statline.
-}
dancingRats_040a :: CardDef
dancingRats_040a =
  ( enemy
      ":the-symphony-of-erich-zann:040a"
      ("Dancing Rats" <:> "Requiem Mass")
      Set.TheSymphonyOfErichZann
      1
  )
    { cdCardTraits = singleton Creature
    , cdFight = fight 1
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }

dancingRats_040b :: CardDef
dancingRats_040b =
  ( enemy
      ":the-symphony-of-erich-zann:040b"
      ("Dancing Rats" <:> "Romantic Harmony")
      Set.TheSymphonyOfErichZann
      1
  )
    { cdCardTraits = singleton Creature
    , cdFight = fight 1
    , cdHealth = health 1
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }

dancingRats_040c :: CardDef
dancingRats_040c =
  ( enemy
      ":the-symphony-of-erich-zann:040c"
      ("Dancing Rats" <:> "Sonorous Fanfare")
      Set.TheSymphonyOfErichZann
      1
  )
    { cdCardTraits = singleton Creature
    , cdFight = fight 2
    , cdHealth = health 1
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }

youngNightingale :: CardDef
youngNightingale =
  ( enemy
      ":the-symphony-of-erich-zann:042"
      ("Young Nightingale" <:> "Choir of the Abyss")
      Set.TheSymphonyOfErichZann
      1
  )
    { cdCardTraits = setFromList [Monster, Extradimensional]
    , cdFight = fight 4
    , cdHealth = health 6
    , cdEvade = evade 1
    , cdSanityDamage = sanityDamage 3
    , cdKeywords = singleton Keyword.Aloof
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }
