module Arkham.Location.Cards.ProtoplasmicPool (protoplasmicPool) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ProtoplasmicPool = ProtoplasmicPool LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protoplasmicPool :: LocationCard ProtoplasmicPool
protoplasmicPool = location ProtoplasmicPool Cards.protoplasmicPool 4 (PerPlayer 1)

instance HasAbilities ProtoplasmicPool where
  getAbilities (ProtoplasmicPool attrs) =
    extendRevealed attrs []

instance RunMessage ProtoplasmicPool where
  runMessage msg (ProtoplasmicPool attrs) = runQueueT $ case msg of
    _ -> ProtoplasmicPool <$> liftRunMessage msg attrs
