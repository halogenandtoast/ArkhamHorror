module Arkham.Act.Cards.ChildrenOfBlood.BloodMoney.Bloodbath (bloodbath) where

import Arkham.Act.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Act.Import.Lifted

newtype Bloodbath = Bloodbath ActAttrs
  deriving anyclass (IsAct, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodbath :: ActCard Bloodbath
bloodbath = act (3, A) Bloodbath Cards.bloodbath Nothing

instance RunMessage Bloodbath where
  runMessage msg a@(Bloodbath attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> Bloodbath <$> liftRunMessage msg attrs
