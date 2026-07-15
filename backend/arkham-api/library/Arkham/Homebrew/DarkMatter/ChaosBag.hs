module Arkham.Homebrew.DarkMatter.ChaosBag where

import Arkham.ChaosToken
import Arkham.Difficulty

-- Per the campaign guide's "Assemble the campaign chaos bag". The guide's
-- transcription omits AutoFail/ElderSign; they are included here as every
-- chaos bag contains them.
{- FOURMOLU_DISABLE -}
chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy ->
    [ PlusOne, PlusOne, Zero, Zero, Zero, MinusOne, MinusOne, MinusTwo, MinusTwo
    , Skull, Skull, Cultist, Cultist, Tablet, Tablet, ElderThing, AutoFail, ElderSign
    ]
  Standard ->
    [ PlusOne, Zero, Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusFour
    , Skull, Skull, Cultist, Cultist, Tablet, Tablet, ElderThing, AutoFail, ElderSign
    ]
  Hard ->
    [ Zero, Zero, Zero, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusThree, MinusFour, MinusFive
    , Skull, Skull, Cultist, Cultist, Tablet, Tablet, ElderThing, AutoFail, ElderSign
    ]
  Expert ->
    [ Zero, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusThree, MinusFour, MinusFour, MinusFive, MinusSix, MinusEight
    , Skull, Skull, Cultist, Cultist, Tablet, Tablet, ElderThing, AutoFail, ElderSign
    ]
{- FOURMOLU_ENABLE -}
