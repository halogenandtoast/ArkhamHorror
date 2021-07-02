module Arkham.Types.Location.Cards.StMarysHospital
  ( StMarysHospital(..)
  , stMarysHospital
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (stMarysHospital)
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
import Arkham.Types.Window

newtype StMarysHospital = StMarysHospital LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stMarysHospital :: LocationId -> StMarysHospital
stMarysHospital = StMarysHospital . baseAttrs
  Cards.stMarysHospital
  2
  (PerPlayer 1)
  Plus
  [Diamond, Square]

instance HasModifiersFor env StMarysHospital where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance ActionRunner env => HasActions env StMarysHospital where
  getActions iid NonFast (StMarysHospital attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `member` locationInvestigators
      ]
  getActions iid window (StMarysHospital attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env StMarysHospital where
  runMessage msg l@(StMarysHospital attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage (HealDamage (InvestigatorTarget iid) 3)
    _ -> StMarysHospital <$> runMessage msg attrs
