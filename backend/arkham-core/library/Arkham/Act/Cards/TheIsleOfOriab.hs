module Arkham.Act.Cards.TheIsleOfOriab (TheIsleOfOriab (..), theIsleOfOriab) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Port))

newtype TheIsleOfOriab = TheIsleOfOriab ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theIsleOfOriab :: ActCard TheIsleOfOriab
theIsleOfOriab = act (2, A) TheIsleOfOriab Cards.theIsleOfOriab Nothing

instance HasAbilities TheIsleOfOriab where
  getAbilities (TheIsleOfOriab x) =
    [ restrictedAbility x 1 (EachUndefeatedInvestigator $ InvestigatorAt $ LocationWithTrait Port)
        $ Objective
        $ ReactionAbility (RoundEnds #when) Free
    ]

instance RunMessage TheIsleOfOriab where
  runMessage msg (TheIsleOfOriab attrs) = TheIsleOfOriab <$> runMessage msg attrs
