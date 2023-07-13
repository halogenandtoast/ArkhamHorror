module Arkham.Campaigns.TheForgottenAge.ChaosBag where

import Arkham.Difficulty
import Arkham.ChaosToken

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
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Standard ->
    [ PlusOne
    , Zero
    , Zero
    , Zero
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusFive
    , Skull
    , Skull
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Hard ->
    [ PlusOne
    , Zero
    , Zero
    , MinusOne
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusSix
    , Skull
    , Skull
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Expert ->
    [ Zero
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusSix
    , MinusEight
    , Skull
    , Skull
    , ElderThing
    , AutoFail
    , ElderSign
    ]
