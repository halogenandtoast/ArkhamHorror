module Arkham.Act.Cards.OpenThePathAbove (openThePathAbove) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher

newtype OpenThePathAbove = OpenThePathAbove ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openThePathAbove :: ActCard OpenThePathAbove
openThePathAbove = act (3, A) OpenThePathAbove Cards.openThePathAbove Nothing

instance HasAbilities OpenThePathAbove where
  getAbilities (OpenThePathAbove x) =
    [ restricted x 1 (EachUndefeatedInvestigator $ at_ $ "Abbey Tower" <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    | onSide A x
    ]

instance RunMessage OpenThePathAbove where
  runMessage msg a@(OpenThePathAbove attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> OpenThePathAbove <$> liftRunMessage msg attrs
