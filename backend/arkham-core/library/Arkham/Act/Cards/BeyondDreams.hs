module Arkham.Act.Cards.BeyondDreams
  ( BeyondDreams(..)
  , beyondDreams
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype BeyondDreams = BeyondDreams ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beyondDreams :: ActCard BeyondDreams
beyondDreams = act (3, A) BeyondDreams Cards.beyondDreams Nothing

instance RunMessage BeyondDreams where
  runMessage msg a@(BeyondDreams attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> BeyondDreams <$> lift (runMessage msg attrs)
