module Arkham.Location.Cards.ColdWastes
  ( coldWastes
  , ColdWastes(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ColdWastes = ColdWastes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldWastes :: LocationCard ColdWastes
coldWastes = location ColdWastes Cards.coldWastes 2 (PerPlayer 1)

instance HasAbilities ColdWastes where
  getAbilities (ColdWastes attrs) =
    extendRevealed attrs []

instance RunMessage ColdWastes where
  runMessage msg (ColdWastes attrs) = runQueueT $ case msg of
    _ -> ColdWastes <$> lift (runMessage msg attrs)
