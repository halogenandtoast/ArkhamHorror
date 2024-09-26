module Arkham.Asset.Cards.JoeSargentRattletrapBusDriver
  ( joeSargentRattletrapBusDriver
  , JoeSargentRattletrapBusDriver(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype JoeSargentRattletrapBusDriver = JoeSargentRattletrapBusDriver AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeSargentRattletrapBusDriver :: AssetCard JoeSargentRattletrapBusDriver
joeSargentRattletrapBusDriver = asset JoeSargentRattletrapBusDriver Cards.joeSargentRattletrapBusDriver

instance RunMessage JoeSargentRattletrapBusDriver where
  runMessage msg (JoeSargentRattletrapBusDriver attrs) = runQueueT $ case msg of
    _ -> JoeSargentRattletrapBusDriver <$> liftRunMessage msg attrs
