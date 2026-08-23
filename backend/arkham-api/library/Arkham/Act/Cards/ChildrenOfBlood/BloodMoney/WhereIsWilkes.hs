module Arkham.Act.Cards.ChildrenOfBlood.BloodMoney.WhereIsWilkes (whereIsWilkes) where

import Arkham.Act.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Act.Import.Lifted

newtype WhereIsWilkes = WhereIsWilkes ActAttrs
  deriving anyclass (IsAct, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whereIsWilkes :: ActCard WhereIsWilkes
whereIsWilkes = act (2, A) WhereIsWilkes Cards.whereIsWilkes Nothing

instance RunMessage WhereIsWilkes where
  runMessage msg a@(WhereIsWilkes attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> WhereIsWilkes <$> liftRunMessage msg attrs
