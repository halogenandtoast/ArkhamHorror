module Arkham.Homebrew.AgesUnwound.Locations.WhatCouldNeverBe_200 (
  whatCouldNeverBe_200,
) where

import Arkham.Helpers.Modifiers (ModifierType (CannotInvestigate), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)

newtype WhatCouldNeverBe_200 = WhatCouldNeverBe_200 LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatCouldNeverBe_200 :: LocationCard WhatCouldNeverBe_200
whatCouldNeverBe_200 =
  symbolLabel
    $ locationWith WhatCouldNeverBe_200 Cards.whatCouldNeverBe_200 2 (PerPlayer 1)
    $ revealedL
    .~ True

{- | "This location cannot be investigated."

The clues printed on it are therefore only reachable through another card -- the
act 2 ward, /Fulcrum of Possibility/'s shuffle, the agenda's clue placement --
which is the point of its shroud of 2.
-}
instance HasModifiersFor WhatCouldNeverBe_200 where
  getModifiersFor (WhatCouldNeverBe_200 a) = modifySelf a [CannotInvestigate]

instance RunMessage WhatCouldNeverBe_200 where
  runMessage msg (WhatCouldNeverBe_200 attrs) =
    WhatCouldNeverBe_200 <$> runQueueT (liftRunMessage msg attrs)
