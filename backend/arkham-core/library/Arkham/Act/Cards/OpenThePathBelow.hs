module Arkham.Act.Cards.OpenThePathBelow
  ( OpenThePathBelow(..)
  , openThePathBelow
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Resolution
import Arkham.Message
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Ability

newtype OpenThePathBelow = OpenThePathBelow ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openThePathBelow :: ActCard OpenThePathBelow
openThePathBelow = act (3, A) OpenThePathBelow Cards.openThePathBelow Nothing

instance HasAbilities OpenThePathBelow where
  getAbilities (OpenThePathBelow x) =
    [ restrictedAbility
        x
        1
        (EachUndefeatedInvestigator $ InvestigatorAt $
          LocationWithTitle "Chapel of St. Aubert" <> LocationWithoutClues
        )
      $ Objective
      $ ForcedAbility AnyWindow
    ]

instance RunMessage OpenThePathBelow where
  runMessage msg a@(OpenThePathBelow attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) _ 1 _ -> do
      push (AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther)
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 1
      pure a
    _ -> OpenThePathBelow <$> runMessage msg attrs
