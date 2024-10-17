module Arkham.Act.Cards.FindingThePath
  ( FindingThePath(..)
  , findingThePath
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FindingThePath = FindingThePath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

findingThePath :: ActCard FindingThePath
findingThePath = act (2, A) FindingThePath Cards.findingThePath Nothing

instance RunMessage FindingThePath where
  runMessage msg a@(FindingThePath attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> FindingThePath <$> liftRunMessage msg attrs
