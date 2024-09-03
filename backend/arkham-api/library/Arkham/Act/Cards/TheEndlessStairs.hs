module Arkham.Act.Cards.TheEndlessStairs (TheEndlessStairs (..), theEndlessStairs) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude

newtype TheEndlessStairs = TheEndlessStairs ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEndlessStairs :: ActCard TheEndlessStairs
theEndlessStairs = act (2, A) TheEndlessStairs Cards.theEndlessStairs Nothing

instance HasAbilities TheEndlessStairs where
  getAbilities (TheEndlessStairs x) =
    restrictedAbility
      (proxied (RevealedLocation <> LocationWithLabel "mysteriousStairs5") x)
      1
      Here
      (ActionAbility [Action.Resign] $ ActionCost 1)
      : [ restrictedAbility x 2 AllUndefeatedInvestigatorsResigned
          $ Objective
          $ ForcedAbility AnyWindow
        | onSide A x
        ]

instance RunMessage TheEndlessStairs where
  runMessage msg a@(TheEndlessStairs attrs) = case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      push $ Resign iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push R1
      pure a
    _ -> TheEndlessStairs <$> runMessage msg attrs
