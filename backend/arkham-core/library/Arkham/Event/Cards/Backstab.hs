module Arkham.Event.Cards.Backstab where

import Arkham.Aspect
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Fight
import Arkham.Prelude

newtype Backstab = Backstab EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab :: EventCard Backstab
backstab = event Backstab Cards.backstab

instance RunMessage Backstab where
  runMessage msg e@(Backstab attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      chooseFight <- leftOr <$> aspect iid attrs (#agility `InsteadOf` #combat) (mkChooseFight iid attrs)
      pushAll $ skillTestModifier attrs iid (DamageDealt 2) : chooseFight
      pure e
    _ -> Backstab <$> runMessage msg attrs
