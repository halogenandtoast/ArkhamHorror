module Arkham.Act.Cards.TheDoomThatCameBefore (TheDoomThatCameBefore (..), theDoomThatCameBefore) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Prelude

newtype TheDoomThatCameBefore = TheDoomThatCameBefore ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theDoomThatCameBefore :: ActCard TheDoomThatCameBefore
theDoomThatCameBefore = act (2, A) TheDoomThatCameBefore Cards.theDoomThatCameBefore Nothing

instance RunMessage TheDoomThatCameBefore where
  runMessage msg (TheDoomThatCameBefore attrs) = TheDoomThatCameBefore <$> runMessage msg attrs
