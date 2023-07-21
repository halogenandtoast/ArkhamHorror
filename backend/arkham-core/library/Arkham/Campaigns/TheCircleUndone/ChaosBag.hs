module Arkham.Campaigns.TheCircleUndone.ChaosBag where

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
    , MinusTwo
    , MinusThree
    , Skull
    , Skull
    , AutoFail
    , ElderSign
    ]
  Standard ->
    [ PlusOne
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusFour
    , Skull
    , Skull
    , AutoFail
    , ElderSign
    ]
  Hard ->
    [ Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusFour
    , MinusFive
    , Skull
    , Skull
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
    , MinusFour
    , MinusSix
    , MinusEight
    , Skull
    , Skull
    , AutoFail
    , ElderSign
    ]
