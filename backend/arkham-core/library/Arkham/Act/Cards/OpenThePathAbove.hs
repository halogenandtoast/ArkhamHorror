module Arkham.Act.Cards.OpenThePathAbove (
  OpenThePathAbove (..),
  openThePathAbove,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution

newtype OpenThePathAbove = OpenThePathAbove ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openThePathAbove :: ActCard OpenThePathAbove
openThePathAbove = act (3, A) OpenThePathAbove Cards.openThePathAbove Nothing

instance HasAbilities OpenThePathAbove where
  getAbilities (OpenThePathAbove x)
    | onSide A x =
        [ restrictedAbility
            x
            1
            ( EachUndefeatedInvestigator $
                InvestigatorAt $
                  LocationWithTitle "Abbey Tower"
                    <> LocationWithoutClues
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage OpenThePathAbove where
  runMessage msg a@(OpenThePathAbove attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push (AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther)
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 2
      pure a
    _ -> OpenThePathAbove <$> runMessage msg attrs
