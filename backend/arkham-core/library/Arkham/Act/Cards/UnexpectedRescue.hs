module Arkham.Act.Cards.UnexpectedRescue (UnexpectedRescue (..), unexpectedRescue) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype UnexpectedRescue = UnexpectedRescue ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

unexpectedRescue :: ActCard UnexpectedRescue
unexpectedRescue = act (4, A) UnexpectedRescue Cards.unexpectedRescue Nothing

instance RunMessage UnexpectedRescue where
  runMessage msg a@(UnexpectedRescue attrs) = case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> UnexpectedRescue <$> runMessage msg attrs
