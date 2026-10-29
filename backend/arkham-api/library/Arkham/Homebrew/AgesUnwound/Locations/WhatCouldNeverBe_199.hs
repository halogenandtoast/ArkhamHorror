module Arkham.Homebrew.AgesUnwound.Locations.WhatCouldNeverBe_199 (whatCouldNeverBe_199) where

import Arkham.Helpers.Modifiers (ModifierType (CannotInvestigate), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype WhatCouldNeverBe_199 = WhatCouldNeverBe_199 LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

whatCouldNeverBe_199 :: LocationCard WhatCouldNeverBe_199
whatCouldNeverBe_199 =
  symbolLabel $ location WhatCouldNeverBe_199 Cards.whatCouldNeverBe_199 2 (PerPlayer 1)

instance HasModifiersFor WhatCouldNeverBe_199 where
  getModifiersFor (WhatCouldNeverBe_199 a) = modifySelf a [CannotInvestigate]
