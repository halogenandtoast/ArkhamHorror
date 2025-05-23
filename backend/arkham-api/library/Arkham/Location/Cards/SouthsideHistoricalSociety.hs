module Arkham.Location.Cards.SouthsideHistoricalSociety (southsideHistoricalSociety) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (southsideHistoricalSociety)
import Arkham.Location.Import.Lifted

newtype SouthsideHistoricalSociety = SouthsideHistoricalSociety LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southsideHistoricalSociety :: LocationCard SouthsideHistoricalSociety
southsideHistoricalSociety = location SouthsideHistoricalSociety Cards.southsideHistoricalSociety 3 (PerPlayer 1)

instance HasAbilities SouthsideHistoricalSociety where
  getAbilities (SouthsideHistoricalSociety x) =
    extendRevealed1 x $ playerLimit PerGame $ restricted x 1 (Here <> CanDrawCards) actionAbility

instance RunMessage SouthsideHistoricalSociety where
  runMessage msg l@(SouthsideHistoricalSociety attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 3
      pure l
    _ -> SouthsideHistoricalSociety <$> liftRunMessage msg attrs
