module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Set
import Arkham.Keyword qualified as Keyword

-- | innsmouth_haze. Stands in for Winged One (07094) when Fog over Innsmouth is replaced.
immaterialOne :: CardDef
immaterialOne =
  (enemy ":return-to-the-innsmouth-conspiracy:056" "Immaterial One" Set.InnsmouthHaze 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evadeX
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
    , cdVictoryPoints = Just 1
    }

-- | stalkers_of_cthulhu. Stands in for Young Deep One (07087) when Agents of Cthulhu is replaced.
deepOneAmbusher :: CardDef
deepOneAmbusher =
  (enemy ":return-to-the-innsmouth-conspiracy:063" "Deep One Ambusher" Set.StalkersOfCthulhu 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

{- | return_to_a_light_in_the_fog. Damage and horror are not in the transcription; 1/1
matches the Deep One baseline and the other two enemies in this box.
-}
deepOneGrappler :: CardDef
deepOneGrappler =
  (enemy ":return-to-the-innsmouth-conspiracy:042" "Deep One Grappler" Set.ReturnToALightInTheFog 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
