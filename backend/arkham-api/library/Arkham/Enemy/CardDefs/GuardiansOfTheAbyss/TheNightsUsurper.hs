module Arkham.Enemy.CardDefs.GuardiansOfTheAbyss.TheNightsUsurper where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

dreadedShantak :: CardDef
dreadedShantak =
  (enemy "83029" "Dreaded Shantak" TheNightsUsurper 2)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Dreamlands, Shantak]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    }

speakerForTheDarkPharaoh :: CardDef
speakerForTheDarkPharaoh =
  (enemy "83030" "Speaker for the Dark Pharaoh" TheNightsUsurper 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

xzharah :: CardDef
xzharah =
  unique
    $ doubleSided "83027b"
    $ (enemy "83027a" ("Xzharah" <:> "Chosen of the Beast") TheNightsUsurper 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Servitor, Dreamlands, Conspirator, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      }
