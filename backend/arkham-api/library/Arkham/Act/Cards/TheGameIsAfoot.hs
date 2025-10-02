module Arkham.Act.Cards.TheGameIsAfoot (theGameIsAfoot) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheGameIsAfoot = TheGameIsAfoot ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGameIsAfoot :: ActCard TheGameIsAfoot
theGameIsAfoot = act (2, A) TheGameIsAfoot Cards.theGameIsAfoot Nothing

instance RunMessage TheGameIsAfoot where
  runMessage msg a@(TheGameIsAfoot attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheGameIsAfoot <$> liftRunMessage msg attrs
