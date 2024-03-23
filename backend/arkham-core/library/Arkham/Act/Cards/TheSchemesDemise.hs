module Arkham.Act.Cards.TheSchemesDemise
  ( TheSchemesDemise(..)
  , theSchemesDemise
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheSchemesDemise = TheSchemesDemise ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theSchemesDemise :: ActCard TheSchemesDemise
theSchemesDemise = act (3, A) TheSchemesDemise Cards.theSchemesDemise Nothing

instance RunMessage TheSchemesDemise where
  runMessage msg a@(TheSchemesDemise attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheSchemesDemise <$> lift (runMessage msg attrs)
