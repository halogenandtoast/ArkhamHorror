module Arkham.Act.Cards.CampusSafety (campusSafety) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype CampusSafety = CampusSafety ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

campusSafety :: ActCard CampusSafety
campusSafety = act (3, A) CampusSafety Cards.campusSafety Nothing

{- | Missing HasAbilities?
Campus Safety has an Objective but it is triggered by other cards so this is
left off of this definition
-}
instance RunMessage CampusSafety where
  runMessage msg a@(CampusSafety attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R3
      pure a
    _ -> CampusSafety <$> liftRunMessage msg attrs
