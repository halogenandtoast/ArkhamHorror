module Arkham.Act.Cards.TheWeaverOfTheCosmos
  ( TheWeaverOfTheCosmos(..)
  , theWeaverOfTheCosmos
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheWeaverOfTheCosmos = TheWeaverOfTheCosmos ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theWeaverOfTheCosmos :: ActCard TheWeaverOfTheCosmos
theWeaverOfTheCosmos = act (2, A) TheWeaverOfTheCosmos Cards.theWeaverOfTheCosmos Nothing

instance RunMessage TheWeaverOfTheCosmos where
  runMessage msg a@(TheWeaverOfTheCosmos attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheWeaverOfTheCosmos <$> lift (runMessage msg attrs)
