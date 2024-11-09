module Arkham.Location.Cards.LairOfDagonIntoTheMaelstrom
  ( lairOfDagonIntoTheMaelstrom
  , LairOfDagonIntoTheMaelstrom(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LairOfDagonIntoTheMaelstrom = LairOfDagonIntoTheMaelstrom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lairOfDagonIntoTheMaelstrom :: LocationCard LairOfDagonIntoTheMaelstrom
lairOfDagonIntoTheMaelstrom = location LairOfDagonIntoTheMaelstrom Cards.lairOfDagonIntoTheMaelstrom 0 (Static 0)

instance HasAbilities LairOfDagonIntoTheMaelstrom where
  getAbilities (LairOfDagonIntoTheMaelstrom attrs) =
    extendRevealed attrs []

instance RunMessage LairOfDagonIntoTheMaelstrom where
  runMessage msg (LairOfDagonIntoTheMaelstrom attrs) = runQueueT $ case msg of
    _ -> LairOfDagonIntoTheMaelstrom <$> liftRunMessage msg attrs
