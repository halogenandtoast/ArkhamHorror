module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

zamacona :: CardDef
zamacona =
  (weakness "10011" "Zamacona")
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = setFromList [Keyword.Elusive]
    }

weepingYurei :: CardDef
weepingYurei =
  (weakness "10014" "Weeping Yurei")
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Elusive, Keyword.Hunter]
    }

biancaDieKatz :: CardDef
biancaDieKatz =
  (weakness "10063" "Bianca \"Die Katz\"")
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Criminal, Socialite]
    , cdKeywords = setFromList [Keyword.Bonded 1 "10062", Keyword.Hunter]
    , cdVictoryPoints = Just 0
    }

-- Resident enemy sides (the back faces of the Residents asset cards). Each is
-- the flipped, hostile version a resident takes when their Relationship Level
-- is too low during The Final Evening.
