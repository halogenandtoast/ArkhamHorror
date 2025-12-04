module Arkham.Act.Cards.TheLadyWithTheRedParasol (theLadyWithTheRedParasol) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheLadyWithTheRedParasol = TheLadyWithTheRedParasol ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theLadyWithTheRedParasol :: ActCard TheLadyWithTheRedParasol
theLadyWithTheRedParasol = act (1, A) TheLadyWithTheRedParasol Cards.theLadyWithTheRedParasol Nothing

instance RunMessage TheLadyWithTheRedParasol where
  runMessage msg a@(TheLadyWithTheRedParasol attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheLadyWithTheRedParasol <$> liftRunMessage msg attrs
