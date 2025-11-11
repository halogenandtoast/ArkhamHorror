module Arkham.Act.Cards.CasingTheJoint (casingTheJoint) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype CasingTheJoint = CasingTheJoint ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

casingTheJoint :: ActCard CasingTheJoint
casingTheJoint = act (1, A) CasingTheJoint Cards.casingTheJoint Nothing

instance RunMessage CasingTheJoint where
  runMessage msg a@(CasingTheJoint attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> CasingTheJoint <$> liftRunMessage msg attrs
