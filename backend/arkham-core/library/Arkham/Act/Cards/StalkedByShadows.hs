module Arkham.Act.Cards.StalkedByShadows
  ( StalkedByShadows(..)
  , stalkedByShadows
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Attrs
import Arkham.Act.Runner
import Arkham.Classes

newtype StalkedByShadows = StalkedByShadows ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkedByShadows :: ActCard StalkedByShadows
stalkedByShadows = act (1, A) StalkedByShadows Cards.stalkedByShadows Nothing

instance ActRunner env => RunMessage env StalkedByShadows where
  runMessage msg (StalkedByShadows attrs) =
    StalkedByShadows <$> runMessage msg attrs
