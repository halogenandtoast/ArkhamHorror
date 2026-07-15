module Arkham.Homebrew.CircusExMortis.ChaosBag where

import Arkham.ChaosToken
import Arkham.Difficulty

-- Per the campaign guide's "Assemble the chaos bag". The Prologue's 3 moon
-- tokens are a campaign mechanic handled by later work.
{- FOURMOLU_DISABLE -}
chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy ->
    [ PlusOne, PlusOne, Zero, Zero, Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo
    , Skull, Skull, ElderThing, AutoFail, ElderSign
    ]
  Standard ->
    [ PlusOne, Zero, Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusFour
    , Skull, Skull, ElderThing, AutoFail, ElderSign
    ]
  Hard ->
    [ Zero, Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusFour, MinusFive
    , Skull, Skull, ElderThing, AutoFail, ElderSign
    ]
  Expert ->
    [ Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusFour, MinusFive, MinusSeven
    , Skull, Skull, ElderThing, AutoFail, ElderSign
    ]
{- FOURMOLU_ENABLE -}
