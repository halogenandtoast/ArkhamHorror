module Arkham.Location.Cards.SawboneAlley
  ( sawboneAlley
  , SawboneAlley(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SawboneAlley = SawboneAlley LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sawboneAlley :: LocationCard SawboneAlley
sawboneAlley = location SawboneAlley Cards.sawboneAlley 2 (PerPlayer 2)

instance HasAbilities SawboneAlley where
  getAbilities (SawboneAlley attrs) =
    extendRevealed attrs []

instance RunMessage SawboneAlley where
  runMessage msg (SawboneAlley attrs) = runQueueT $ case msg of
    _ -> SawboneAlley <$> liftRunMessage msg attrs
