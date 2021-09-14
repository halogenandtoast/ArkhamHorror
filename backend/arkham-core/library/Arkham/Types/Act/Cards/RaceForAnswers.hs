module Arkham.Types.Act.Cards.RaceForAnswers
  ( RaceForAnswers(..)
  , raceForAnswers
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes

newtype RaceForAnswers = RaceForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

raceForAnswers :: ActCard RaceForAnswers
raceForAnswers = act (1, A) RaceForAnswers Cards.raceForAnswers Nothing

instance ActRunner env => RunMessage env RaceForAnswers where
  runMessage msg (RaceForAnswers attrs) =
    RaceForAnswers <$> runMessage msg attrs
