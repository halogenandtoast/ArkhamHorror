module Arkham.Campaigns.TheDunwichLegacy.ChaosBag where

import Arkham.ChaosToken
import Arkham.Difficulty

chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy ->
    [ PlusOne
    , PlusOne
    , Zero
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , Skull
    , Skull
    , Cultist
    , AutoFail
    , ElderSign
    ]
  Standard ->
    [ PlusOne
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusFour
    , Skull
    , Skull
    , Cultist
    , AutoFail
    , ElderSign
    ]
  Hard ->
    [ Zero
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFive
    , Skull
    , Skull
    , Cultist
    , AutoFail
    , ElderSign
    ]
  Expert ->
    [ Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusFive
    , MinusSix
    , MinusEight
    , Skull
    , Skull
    , Cultist
    , AutoFail
    , ElderSign
    ]
