module Arkham.Types.Location.Cards.SouthsideMasBoardingHouse
  ( SouthsideMasBoardingHouse(..)
  , southsideMasBoardingHouse
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (southsideMasBoardingHouse)
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
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype SouthsideMasBoardingHouse = SouthsideMasBoardingHouse LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southsideMasBoardingHouse :: LocationId -> SouthsideMasBoardingHouse
southsideMasBoardingHouse = SouthsideMasBoardingHouse . baseAttrs
  Cards.southsideMasBoardingHouse
  2
  (PerPlayer 1)
  Square
  [Diamond, Plus, Circle]

instance HasModifiersFor env SouthsideMasBoardingHouse where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance ActionRunner env => HasActions env SouthsideMasBoardingHouse where
  getActions iid NonFast (SouthsideMasBoardingHouse attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `member` locationInvestigators
      ]
  getActions iid window (SouthsideMasBoardingHouse attrs) =
    getActions iid window attrs

instance (LocationRunner env) => RunMessage env SouthsideMasBoardingHouse where
  runMessage msg l@(SouthsideMasBoardingHouse attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage
        (SearchDeckForTraits iid (InvestigatorTarget iid) [Ally])
    _ -> SouthsideMasBoardingHouse <$> runMessage msg attrs
