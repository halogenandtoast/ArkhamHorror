module Arkham.Act.Cards.EscapingTheOtherworld (escapingTheOtherworld) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype EscapingTheOtherworld = EscapingTheOtherworld ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

escapingTheOtherworld :: ActCard EscapingTheOtherworld
escapingTheOtherworld = act (3, A) EscapingTheOtherworld Cards.escapingTheOtherworld Nothing

instance RunMessage EscapingTheOtherworld where
  runMessage msg a@(EscapingTheOtherworld attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> EscapingTheOtherworld <$> liftRunMessage msg attrs
