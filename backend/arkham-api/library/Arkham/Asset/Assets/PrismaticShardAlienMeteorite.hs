module Arkham.Asset.Assets.PrismaticShardAlienMeteorite (prismaticShardAlienMeteorite) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype PrismaticShardAlienMeteorite = PrismaticShardAlienMeteorite AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prismaticShardAlienMeteorite :: AssetCard PrismaticShardAlienMeteorite
prismaticShardAlienMeteorite = asset PrismaticShardAlienMeteorite Cards.prismaticShardAlienMeteorite

instance RunMessage PrismaticShardAlienMeteorite where
  runMessage msg (PrismaticShardAlienMeteorite attrs) = runQueueT $ case msg of
    _ -> PrismaticShardAlienMeteorite <$> liftRunMessage msg attrs
