module Arkham.Act.Cards.BackIntoTheDepths
  ( BackIntoTheDepths(..)
  , backIntoTheDepths
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype BackIntoTheDepths = BackIntoTheDepths ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

backIntoTheDepths :: ActCard BackIntoTheDepths
backIntoTheDepths = act (1, A) BackIntoTheDepths Cards.backIntoTheDepths Nothing

instance RunMessage BackIntoTheDepths where
  runMessage msg a@(BackIntoTheDepths attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> BackIntoTheDepths <$> liftRunMessage msg attrs
