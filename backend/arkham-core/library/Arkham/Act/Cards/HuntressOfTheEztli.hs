module Arkham.Act.Cards.HuntressOfTheEztli
  ( HuntressOfTheEztli(..)
  , huntressOfTheEztli
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype HuntressOfTheEztli = HuntressOfTheEztli ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntressOfTheEztli :: ActCard HuntressOfTheEztli
huntressOfTheEztli = act (2, A) HuntressOfTheEztli Cards.huntressOfTheEztli Nothing

instance RunMessage HuntressOfTheEztli where
  runMessage msg (HuntressOfTheEztli attrs) = HuntressOfTheEztli <$> runMessage msg attrs
