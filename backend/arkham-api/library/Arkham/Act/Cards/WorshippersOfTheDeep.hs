module Arkham.Act.Cards.WorshippersOfTheDeep
  ( WorshippersOfTheDeep(..)
  , worshippersOfTheDeep
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype WorshippersOfTheDeep = WorshippersOfTheDeep ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

worshippersOfTheDeep :: ActCard WorshippersOfTheDeep
worshippersOfTheDeep = act (3, A) WorshippersOfTheDeep Cards.worshippersOfTheDeep Nothing

instance RunMessage WorshippersOfTheDeep where
  runMessage msg a@(WorshippersOfTheDeep attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> WorshippersOfTheDeep <$> liftRunMessage msg attrs
