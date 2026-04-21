module Arkham.Act.Cards.StrangeInfestation (strangeInfestation) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype StrangeInfestation = StrangeInfestation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

strangeInfestation :: ActCard StrangeInfestation
strangeInfestation = act (1, A) StrangeInfestation Cards.strangeInfestation Nothing

instance RunMessage StrangeInfestation where
  runMessage msg a@(StrangeInfestation attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> StrangeInfestation <$> liftRunMessage msg attrs
