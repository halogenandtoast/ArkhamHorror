{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Shared constructors that several enemy card-def modules build on.
module Arkham.Enemy.CardDefs.Helpers where

import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.EncounterSet hiding (
  Arkham,
  Blight,
  Byakhee,
  Dreamlands,
  Dunwich,
  Expedition,
  Poison,
  Rlyeh,
  StarSpawn,
 )
import Arkham.Enemy.CardDefs.Base
import Arkham.GameValue
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher hiding (IgnoreChaosToken)
import Arkham.Modifier
import Arkham.Name
import Arkham.Prelude
import Arkham.Trait

longestNightBack :: Map Text Value
longestNightBack = mapFromList [("customBack", String "back_the_longest_night.jpg")]

replicatingAberration :: CardCode -> CardDef
replicatingAberration code =
  (enemy code "Replicating Aberration" TheBlobThatAteEverythingELSE 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Ooze, Manifold]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.ScenarioKeywordX "Blob" 3]
    }
