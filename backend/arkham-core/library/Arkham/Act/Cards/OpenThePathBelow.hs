module Arkham.Act.Cards.OpenThePathBelow (
  OpenThePathBelow (..),
  openThePathBelow,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution

newtype OpenThePathBelow = OpenThePathBelow ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openThePathBelow :: ActCard OpenThePathBelow
openThePathBelow = act (3, A) OpenThePathBelow Cards.openThePathBelow Nothing

instance HasAbilities OpenThePathBelow where
  getAbilities (OpenThePathBelow x)
    | onSide A x =
        [ restrictedAbility
            x
            1
            ( EachUndefeatedInvestigator $
                InvestigatorAt $
                  LocationWithTitle "Chapel of St. Aubert"
                    <> LocationWithoutClues
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage OpenThePathBelow where
  runMessage msg a@(OpenThePathBelow attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push (AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther)
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 1
      pure a
    _ -> OpenThePathBelow <$> runMessage msg attrs
