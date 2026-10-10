module Arkham.Homebrew.AgesUnwound.ChaosBag where

import Arkham.ChaosToken
import Arkham.Difficulty

{- | The four printed bags.

Two things in here look like transcription errors and are not: there is **no**
'ElderThing' at any difficulty, and every bag holds **three** 'Skull' rather
than the usual two. Both were confirmed by the campaign owner; do not "fix"
either (see @docs/homebrew/data/ages-unwound-structure.md@).

Later steps change the bag permanently: Scenario IV and Scenario VI each add a
difficulty-scaled negative token for the rest of the campaign.
-}
{- FOURMOLU_DISABLE -}
chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy ->
    [ PlusOne, PlusOne, Zero, Zero, Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo
    , Skull, Skull, Skull, Cultist, Tablet, ElderSign, AutoFail
    ]
  Standard ->
    [ PlusOne, Zero, Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusFour
    , Skull, Skull, Skull, Cultist, Tablet, ElderSign, AutoFail
    ]
  Hard ->
    [ Zero, Zero, Zero, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusThree, MinusFour, MinusFive
    , Skull, Skull, Skull, Cultist, Tablet, ElderSign, AutoFail
    ]
  Expert ->
    [ Zero, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusThree, MinusFour, MinusFour, MinusFive, MinusSix, MinusEight
    , Skull, Skull, Skull, Cultist, Tablet, ElderSign, AutoFail
    ]
{- FOURMOLU_ENABLE -}
