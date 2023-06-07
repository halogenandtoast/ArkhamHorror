module Arkham.Act.Cards.WarmWelcome
  ( WarmWelcome(..)
  , warmWelcome
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype WarmWelcome = WarmWelcome ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

warmWelcome :: ActCard WarmWelcome
warmWelcome = act (1, A) WarmWelcome Cards.warmWelcome Nothing

instance RunMessage WarmWelcome where
  runMessage msg (WarmWelcome attrs) = WarmWelcome <$> runMessage msg attrs
