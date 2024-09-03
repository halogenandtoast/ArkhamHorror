module Arkham.Location.Cards.SouthsideHistoricalSociety (
  SouthsideHistoricalSociety (..),
  southsideHistoricalSociety,
) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (southsideHistoricalSociety)
import Arkham.Location.Runner
import Arkham.Prelude

newtype SouthsideHistoricalSociety = SouthsideHistoricalSociety LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southsideHistoricalSociety :: LocationCard SouthsideHistoricalSociety
southsideHistoricalSociety = location SouthsideHistoricalSociety Cards.southsideHistoricalSociety 3 (PerPlayer 1)

instance HasAbilities SouthsideHistoricalSociety where
  getAbilities (SouthsideHistoricalSociety x) =
    extendRevealed x [playerLimit PerGame $ restrictedAbility x 1 (Here <> CanDrawCards) actionAbility]

instance RunMessage SouthsideHistoricalSociety where
  runMessage msg l@(SouthsideHistoricalSociety attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ drawCards iid (attrs.ability 1) 3
      pure l
    _ -> SouthsideHistoricalSociety <$> runMessage msg attrs
