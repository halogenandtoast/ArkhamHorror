module Arkham.Types.Location.Cards.SouthsideHistoricalSociety
  ( SouthsideHistoricalSociety(..)
  , southsideHistoricalSociety
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (southsideHistoricalSociety)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message

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

instance HasAbilities env SouthsideHistoricalSociety where
  getAbilities iid window (SouthsideHistoricalSociety x) | locationRevealed x =
    withBaseAbilities iid window x $ pure
      [ restrictedAbility
          x
          1
          (Here <> CanDrawCards)
          (ActionAbility Nothing $ ActionCost 1)
        & abilityLimitL
        .~ PlayerLimit PerGame 1
      ]
  getAbilities iid window (SouthsideHistoricalSociety x) =
    getAbilities iid window x

instance LocationRunner env => RunMessage env SouthsideHistoricalSociety where
  runMessage msg l@(SouthsideHistoricalSociety attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DrawCards iid 3 False)
    _ -> SouthsideHistoricalSociety <$> runMessage msg attrs
