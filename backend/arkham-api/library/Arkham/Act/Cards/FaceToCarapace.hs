module Arkham.Act.Cards.FaceToCarapace (faceToCarapace) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FaceToCarapace = FaceToCarapace ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

faceToCarapace :: ActCard FaceToCarapace
faceToCarapace = act (3, A) FaceToCarapace Cards.faceToCarapace Nothing

instance RunMessage FaceToCarapace where
  runMessage msg a@(FaceToCarapace attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> FaceToCarapace <$> liftRunMessage msg attrs
