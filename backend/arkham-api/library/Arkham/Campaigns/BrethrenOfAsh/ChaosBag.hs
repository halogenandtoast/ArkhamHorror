module Arkham.Campaigns.BrethrenOfAsh.ChaosBag where

import Arkham.ChaosToken
import Arkham.Difficulty

{- FOURMOLU_DISABLE -}
chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy ->
    [ PlusOne, PlusOne, Zero, Zero, Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo
    , Skull, Skull, ElderThing, Tablet, AutoFail, ElderSign
    ]
  Standard ->
    [ PlusOne, Zero, Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusFour
    , Skull, Skull, ElderThing, Tablet, AutoFail, ElderSign
    ]
  Hard ->
    [ Zero, Zero, Zero, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusThree, MinusFour, MinusFive
    , Skull, Skull, ElderThing, Tablet, AutoFail, ElderSign
    ]
  Expert ->
    [ Zero, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusThree, MinusFour, MinusFour, MinusFive, MinusSix, MinusEight
    , Skull, Skull, ElderThing, Tablet, AutoFail, ElderSign
    ]
{- FOURMOLU_ENABLE -}
