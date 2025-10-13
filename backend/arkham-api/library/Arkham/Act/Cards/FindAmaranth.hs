module Arkham.Act.Cards.FindAmaranth (findAmaranth) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FindAmaranth = FindAmaranth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

findAmaranth :: ActCard FindAmaranth
findAmaranth = act (1, A) FindAmaranth Cards.findAmaranth Nothing

instance RunMessage FindAmaranth where
  runMessage msg a@(FindAmaranth attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> FindAmaranth <$> liftRunMessage msg attrs
