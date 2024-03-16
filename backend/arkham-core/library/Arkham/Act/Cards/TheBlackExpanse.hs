module Arkham.Act.Cards.TheBlackExpanse
  ( TheBlackExpanse(..)
  , theBlackExpanse
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheBlackExpanse = TheBlackExpanse ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theBlackExpanse :: ActCard TheBlackExpanse
theBlackExpanse = act (3, A) TheBlackExpanse Cards.theBlackExpanse Nothing

instance RunMessage TheBlackExpanse where
  runMessage msg a@(TheBlackExpanse attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      pure a
    _ -> TheBlackExpanse <$> lift (runMessage msg attrs)
