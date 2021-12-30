module Arkham.Location.Cards.SouthsideHistoricalSociety
  ( SouthsideHistoricalSociety(..)
  , southsideHistoricalSociety
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (southsideHistoricalSociety)
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Message

newtype SouthsideHistoricalSociety = SouthsideHistoricalSociety LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southsideHistoricalSociety :: LocationCard SouthsideHistoricalSociety
southsideHistoricalSociety = location
  SouthsideHistoricalSociety
  Cards.southsideHistoricalSociety
  3
  (PerPlayer 1)
  Square
  [Diamond, Plus, Circle]

instance HasAbilities SouthsideHistoricalSociety where
  getAbilities (SouthsideHistoricalSociety x) | locationRevealed x =
    withBaseAbilities x $
      [ restrictedAbility
          x
          1
          (Here <> CanDrawCards)
          (ActionAbility Nothing $ ActionCost 1)
        & abilityLimitL
        .~ PlayerLimit PerGame 1
      ]
  getAbilities (SouthsideHistoricalSociety x) =
    getAbilities x

instance LocationRunner env => RunMessage env SouthsideHistoricalSociety where
  runMessage msg l@(SouthsideHistoricalSociety attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DrawCards iid 3 False)
    _ -> SouthsideHistoricalSociety <$> runMessage msg attrs
