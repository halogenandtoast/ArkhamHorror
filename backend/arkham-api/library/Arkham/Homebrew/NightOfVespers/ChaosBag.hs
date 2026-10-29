module Arkham.Homebrew.NightOfVespers.ChaosBag where

import Arkham.ChaosToken
import Arkham.Difficulty

-- Campaign guide, page 2. Later campaign changes are not implemented.
chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy ->
    [ PlusOne
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , Skull
    , Skull
    , Skull
    , AutoFail
    , ElderSign
    ]
  Standard ->
    [ PlusOne
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , Skull
    , Skull
    , Cultist
    , Tablet
    , AutoFail
    , ElderSign
    ]
  Hard ->
    [ Zero
    , Zero
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
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Expert ->
    [ Zero
    , MinusOne
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusFive
    , MinusSix
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
