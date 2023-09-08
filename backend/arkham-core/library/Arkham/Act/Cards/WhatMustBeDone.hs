module Arkham.Act.Cards.WhatMustBeDone
  ( WhatMustBeDone(..)
  , whatMustBeDone
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype WhatMustBeDone = WhatMustBeDone ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

whatMustBeDone :: ActCard WhatMustBeDone
whatMustBeDone = act (3, A) WhatMustBeDone Cards.whatMustBeDone Nothing

instance RunMessage WhatMustBeDone where
  runMessage msg (WhatMustBeDone attrs) = WhatMustBeDone <$> runMessage msg attrs
