module Arkham.Act.Cards.OpenThePathBelow (openThePathBelow) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher

newtype OpenThePathBelow = OpenThePathBelow ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openThePathBelow :: ActCard OpenThePathBelow
openThePathBelow = act (3, A) OpenThePathBelow Cards.openThePathBelow Nothing

instance HasAbilities OpenThePathBelow where
  getAbilities (OpenThePathBelow x) =
    [ restricted x 1 (EachUndefeatedInvestigator $ at_ $ "Chapel of St. Aubert" <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    | onSide A x
    ]

instance RunMessage OpenThePathBelow where
  runMessage msg a@(OpenThePathBelow attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> OpenThePathBelow <$> liftRunMessage msg attrs
