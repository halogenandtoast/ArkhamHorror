module Arkham.Location.Cards.SyzygyChamber
  ( syzygyChamber
  , SyzygyChamber(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SyzygyChamber = SyzygyChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

syzygyChamber :: LocationCard SyzygyChamber
syzygyChamber = location SyzygyChamber Cards.syzygyChamber 0 (Static 0)

instance HasAbilities SyzygyChamber where
  getAbilities (SyzygyChamber attrs) =
    extendRevealed attrs []

instance RunMessage SyzygyChamber where
  runMessage msg (SyzygyChamber attrs) = runQueueT $ case msg of
    _ -> SyzygyChamber <$> liftRunMessage msg attrs
