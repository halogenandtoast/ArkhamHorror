module Arkham.Homebrew.AgesUnwound.Locations.WhatCouldNeverBe_199 (
  whatCouldNeverBe_199,
) where

import Arkham.Helpers.Modifiers (ModifierType (CannotInvestigate), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)

newtype WhatCouldNeverBe_199 = WhatCouldNeverBe_199 LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatCouldNeverBe_199 :: LocationCard WhatCouldNeverBe_199
whatCouldNeverBe_199 =
  symbolLabel
    $ locationWith WhatCouldNeverBe_199 Cards.whatCouldNeverBe_199 2 (PerPlayer 1)
    $ revealedL
    .~ True

{- | "This location cannot be investigated."

The clues printed on it are therefore only reachable through another card -- the
act 2 ward, /Fulcrum of Possibility/'s shuffle, the agenda's clue placement --
which is the point of its shroud of 2.
-}
instance HasModifiersFor WhatCouldNeverBe_199 where
  getModifiersFor (WhatCouldNeverBe_199 a) = modifySelf a [CannotInvestigate]

instance RunMessage WhatCouldNeverBe_199 where
  runMessage msg (WhatCouldNeverBe_199 attrs) =
    WhatCouldNeverBe_199 <$> runQueueT (liftRunMessage msg attrs)
