module Arkham.Internal.Scenario where

import Arkham.Internal.ChaosToken
import Arkham.Internal.Types
import Arkham.Types
import Arkham.Util
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro

allScenarios
  :: HashMap ArkhamScenarioCode (ArkhamDifficulty -> ArkhamScenarioInternal)
allScenarios =
  HashMap.fromList [(ArkhamScenarioCode "theGathering", theGathering)]

defaultTokenMap :: HashMap ArkhamChaosToken ArkhamChaosTokenInternal
defaultTokenMap = HashMap.fromList
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
  , (ElderSign, elderSignToken)
  ]

buildTokenMapFrom
  :: HashMap ArkhamChaosToken ArkhamChaosTokenInternal
  -> HashMap ArkhamChaosToken ArkhamChaosTokenInternal
buildTokenMapFrom scenarioTokens = HashMap.union scenarioTokens defaultTokenMap

theGathering :: ArkhamDifficulty -> ArkhamScenarioInternal
theGathering difficulty' = ArkhamScenarioInternal
  { scenarioName = "The Gathering"
  , tokenMap = buildTokenMapFrom $ HashMap.fromList
    [ (Skull, theGatheringSkullToken difficulty')
    , (Cultist, theGatheringCultistToken difficulty')
    , (Tablet, theGatheringTabletToken difficulty')
    ]
  }

isEasyStandard :: ArkhamDifficulty -> Bool
isEasyStandard difficulty' =
  difficulty' == ArkhamEasy || difficulty' == ArkhamStandard

theGatheringSkullToken :: ArkhamDifficulty -> ArkhamChaosTokenInternal
theGatheringSkullToken difficulty' = if isEasyStandard difficulty'
  then token
    { tokenToResult = \g i -> Modifier . countTraitMatch Ghoul $ locationFor i g
    }
  else token
    { tokenToResult = modifier (-2)
    , tokenOnFail = \_ _ -> error "TODO: Draw a ghoul"
    }

theGatheringCultistToken :: ArkhamDifficulty -> ArkhamChaosTokenInternal
theGatheringCultistToken difficulty' = if isEasyStandard difficulty'
  then token
    { tokenToResult = modifier (-1)
    , tokenOnFail = \g _ -> g & player . sanityDamage +~ 1
    }
  else token
    { tokenOnReveal = \_ _ -> error "TODO: Reveal another"
    , tokenOnFail = \g _ -> g & player . sanityDamage +~ 2
    }

theGatheringTabletToken :: ArkhamDifficulty -> ArkhamChaosTokenInternal
theGatheringTabletToken difficulty' = if isEasyStandard difficulty'
  then token
    { tokenToResult = modifier (-2)
    , tokenOnReveal = \g i -> if countTraitMatch Ghoul (locationFor i g) > 0
      then g & player . healthDamage +~ 1
      else g
    }
  else token
    { tokenToResult = modifier (-4)
    , tokenOnReveal = \g i -> if countTraitMatch Ghoul (locationFor i g) > 0
      then g & player . healthDamage +~ 1 & player . sanityDamage +~ 1
      else g
    }
