module Arkham.Act.Cards.TrialOfTheHuntress
  ( TrialOfTheHuntress(..)
  , trialOfTheHuntress
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TrialOfTheHuntress = TrialOfTheHuntress ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trialOfTheHuntress :: ActCard TrialOfTheHuntress
trialOfTheHuntress =
  act (1, E) TrialOfTheHuntress Cards.trialOfTheHuntress Nothing

instance RunMessage TrialOfTheHuntress where
  runMessage msg (TrialOfTheHuntress attrs) =
    TrialOfTheHuntress <$> runMessage msg attrs
