module Arkham.Act.Cards.ChildrenOfBlood.NewHorizons.ToNewHorizons (toNewHorizons) where

import Arkham.Act.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Act.Import.Lifted

newtype ToNewHorizons = ToNewHorizons ActAttrs
  deriving anyclass (IsAct, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toNewHorizons :: ActCard ToNewHorizons
toNewHorizons = act (1, A) ToNewHorizons Cards.toNewHorizons Nothing

instance RunMessage ToNewHorizons where
  runMessage msg a@(ToNewHorizons attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ToNewHorizons <$> liftRunMessage msg attrs
