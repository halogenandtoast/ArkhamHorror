module Arkham.Act.Cards.PursuingShadows
  ( PursuingShadows(..)
  , pursuingShadows
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Attrs
import Arkham.Act.Runner
import Arkham.Classes

newtype PursuingShadows = PursuingShadows ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pursuingShadows :: ActCard PursuingShadows
pursuingShadows = act (1, A) PursuingShadows Cards.pursuingShadows Nothing

instance ActRunner env => RunMessage env PursuingShadows where
  runMessage msg (PursuingShadows attrs) =
    PursuingShadows <$> runMessage msg attrs
