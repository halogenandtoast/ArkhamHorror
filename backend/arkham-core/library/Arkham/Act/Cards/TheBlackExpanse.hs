module Arkham.Act.Cards.TheBlackExpanse (TheBlackExpanse (..), theBlackExpanse) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher
import Arkham.Source

newtype TheBlackExpanse = TheBlackExpanse ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackExpanse :: ActCard TheBlackExpanse
theBlackExpanse = act (3, A) TheBlackExpanse Cards.theBlackExpanse Nothing

instance HasAbilities TheBlackExpanse where
  getAbilities (TheBlackExpanse x) =
    [mkAbility x 1 $ forced $ EnemyDefeated #after Anyone ByAny $ EnemyWithClues $ atLeast 1]

instance RunMessage TheBlackExpanse where
  runMessage msg a@(TheBlackExpanse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ GainClues iid (attrs.ability 1) 1
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> TheBlackExpanse <$> lift (runMessage msg attrs)
