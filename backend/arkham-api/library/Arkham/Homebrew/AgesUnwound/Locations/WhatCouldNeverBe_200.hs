module Arkham.Homebrew.AgesUnwound.Locations.WhatCouldNeverBe_200 (whatCouldNeverBe_200) where

import Arkham.Helpers.Modifiers (ModifierType (CannotInvestigate), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype WhatCouldNeverBe_200 = WhatCouldNeverBe_200 LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

whatCouldNeverBe_200 :: LocationCard WhatCouldNeverBe_200
whatCouldNeverBe_200 =
  symbolLabel $ location WhatCouldNeverBe_200 Cards.whatCouldNeverBe_200 2 (PerPlayer 1)

instance HasModifiersFor WhatCouldNeverBe_200 where
  getModifiersFor (WhatCouldNeverBe_200 a) = modifySelf a [CannotInvestigate]
