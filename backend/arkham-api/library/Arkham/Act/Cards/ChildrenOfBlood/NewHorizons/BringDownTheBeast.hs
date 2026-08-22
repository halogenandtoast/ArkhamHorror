module Arkham.Act.Cards.ChildrenOfBlood.NewHorizons.BringDownTheBeast (bringDownTheBeast) where

import Arkham.Act.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Act.Import.Lifted

newtype BringDownTheBeast = BringDownTheBeast ActAttrs
  deriving anyclass (IsAct, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bringDownTheBeast :: ActCard BringDownTheBeast
bringDownTheBeast = act (3, A) BringDownTheBeast Cards.bringDownTheBeast Nothing

instance RunMessage BringDownTheBeast where
  runMessage msg a@(BringDownTheBeast attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> BringDownTheBeast <$> liftRunMessage msg attrs
