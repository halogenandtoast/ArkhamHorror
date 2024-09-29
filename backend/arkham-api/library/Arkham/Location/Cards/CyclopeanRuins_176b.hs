module Arkham.Location.Cards.CyclopeanRuins_176b
  ( cyclopeanRuins_176b
  , CyclopeanRuins_176b(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CyclopeanRuins_176b = CyclopeanRuins_176b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanRuins_176b :: LocationCard CyclopeanRuins_176b
cyclopeanRuins_176b = location CyclopeanRuins_176b Cards.cyclopeanRuins_176b 1 (PerPlayer 2)

instance HasAbilities CyclopeanRuins_176b where
  getAbilities (CyclopeanRuins_176b attrs) =
    extendRevealed attrs []

instance RunMessage CyclopeanRuins_176b where
  runMessage msg (CyclopeanRuins_176b attrs) = runQueueT $ case msg of
    _ -> CyclopeanRuins_176b <$> liftRunMessage msg attrs
