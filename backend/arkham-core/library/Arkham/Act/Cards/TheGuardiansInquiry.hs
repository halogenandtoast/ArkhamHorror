module Arkham.Act.Cards.TheGuardiansInquiry
  ( TheGuardiansInquiry(..)
  , theGuardiansInquiry
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheGuardiansInquiry = TheGuardiansInquiry ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGuardiansInquiry :: ActCard TheGuardiansInquiry
theGuardiansInquiry =
  act (1, E) TheGuardiansInquiry Cards.theGuardiansInquiry Nothing

instance RunMessage TheGuardiansInquiry where
  runMessage msg (TheGuardiansInquiry attrs) =
    TheGuardiansInquiry <$> runMessage msg attrs
