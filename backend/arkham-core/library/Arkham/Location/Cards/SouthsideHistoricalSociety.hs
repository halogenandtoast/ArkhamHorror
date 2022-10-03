module Arkham.Location.Cards.SouthsideHistoricalSociety
  ( SouthsideHistoricalSociety(..)
  , southsideHistoricalSociety
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( southsideHistoricalSociety )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message

newtype SouthsideHistoricalSociety = SouthsideHistoricalSociety LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southsideHistoricalSociety :: LocationCard SouthsideHistoricalSociety
southsideHistoricalSociety = location
  SouthsideHistoricalSociety
  Cards.southsideHistoricalSociety
  3
  (PerPlayer 1)

instance HasAbilities SouthsideHistoricalSociety where
  getAbilities (SouthsideHistoricalSociety x) | locationRevealed x =
    withBaseAbilities x
      $ [ limitedAbility (PlayerLimit PerGame 1)
          $ restrictedAbility x 1 (Here <> CanDrawCards)
          $ ActionAbility Nothing
          $ ActionCost 1
        ]
  getAbilities (SouthsideHistoricalSociety x) = getAbilities x

instance RunMessage SouthsideHistoricalSociety where
  runMessage msg l@(SouthsideHistoricalSociety attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (DrawCards iid 3 False)
    _ -> SouthsideHistoricalSociety <$> runMessage msg attrs
