module Arkham.Internal.Scenario where

import Arkham.Internal.ChaosToken
import Arkham.Internal.Types
import Arkham.Types
import Arkham.Util
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro

theGathering :: ArkhamDifficulty -> ArkhamScenarioInternal
theGathering _ = ArkhamScenarioInternal
  { scenarioName = "The Gathering"
  , tokenMap = HashMap.fromList
    [ (PlusOne, plusOneToken)
    , (Zero, zeroToken)
    , (MinusOne, minusOneToken)
    , (MinusTwo, minusTwoToken)
    , (MinusThree, minusThreeToken)
    , (MinusFour, minusFourToken)
    , (MinusFive, minusFiveToken)
    , (MinusSix, minusSixToken)
    , (MinusSeven, minusSevenToken)
    , (MinusEight, minusEightToken)
    , (AutoFail, autoFailToken)
    , ( ElderSign
      , elderSignToken
      )
    -- end defaults
    , (Skull, theGatheringSkullToken)
    , (Cultist, theGatheringCultistToken)
    , (Tablet, theGatheringTabletToken)
    ]
  }

theGatheringSkullToken :: ArkhamChaosTokenInternal
theGatheringSkullToken = token
  { tokenToResult = \g i -> Modifier . countTraitMatch Ghoul $ locationFor i g
  }

theGatheringCultistToken :: ArkhamChaosTokenInternal
theGatheringCultistToken = token
  { tokenToResult = const (const $ Modifier (-1))
  , tokenOnFail = \g _ -> g & player . sanityDamage +~ 1
  }

theGatheringTabletToken :: ArkhamChaosTokenInternal
theGatheringTabletToken = token
  { tokenToResult = const (const $ Modifier (-2))
  , tokenOnReveal = \g i -> if countTraitMatch Ghoul (locationFor i g) > 0
    then g & player . healthDamage +~ 1
    else g
  }
