module Arkham.Location.Cards.ChurningWaters
  ( churningWaters
  , ChurningWaters(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ChurningWaters = ChurningWaters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

churningWaters :: LocationCard ChurningWaters
churningWaters = location ChurningWaters Cards.churningWaters 6 (Static 0)

instance HasAbilities ChurningWaters where
  getAbilities (ChurningWaters attrs) =
    extendRevealed attrs []

instance RunMessage ChurningWaters where
  runMessage msg (ChurningWaters attrs) = runQueueT $ case msg of
    _ -> ChurningWaters <$> liftRunMessage msg attrs
