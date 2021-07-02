module Arkham.Types.Location.Cards.SouthsideHistoricalSociety
  ( SouthsideHistoricalSociety(..)
  , southsideHistoricalSociety
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (southsideHistoricalSociety)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Window

newtype SouthsideHistoricalSociety = SouthsideHistoricalSociety LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southsideHistoricalSociety :: LocationId -> SouthsideHistoricalSociety
southsideHistoricalSociety = SouthsideHistoricalSociety . baseAttrs
  Cards.southsideHistoricalSociety
  3
  (PerPlayer 1)
  Square
  [Diamond, Plus, Circle]

instance HasModifiersFor env SouthsideHistoricalSociety where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance ActionRunner env => HasActions env SouthsideHistoricalSociety where
  getActions iid NonFast (SouthsideHistoricalSociety attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `member` locationInvestigators
      ]
  getActions iid window (SouthsideHistoricalSociety attrs) =
    getActions iid window attrs

instance (LocationRunner env) => RunMessage env SouthsideHistoricalSociety where
  runMessage msg l@(SouthsideHistoricalSociety attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage (DrawCards iid 3 False)
    _ -> SouthsideHistoricalSociety <$> runMessage msg attrs
