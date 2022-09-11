module Arkham.Act.Cards.AlejandrosPrison
  ( AlejandrosPrison(..)
  , alejandrosPrison
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype AlejandrosPrison = AlejandrosPrison ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

alejandrosPrison :: ActCard AlejandrosPrison
alejandrosPrison = act (3, C) AlejandrosPrison Cards.alejandrosPrison Nothing

instance RunMessage AlejandrosPrison where
  runMessage msg (AlejandrosPrison attrs) =
    AlejandrosPrison <$> runMessage msg attrs
