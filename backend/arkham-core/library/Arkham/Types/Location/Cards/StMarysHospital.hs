module Arkham.Types.Location.Cards.StMarysHospital
  ( StMarysHospital(..)
  , stMarysHospital
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (stMarysHospital)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype StMarysHospital = StMarysHospital LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stMarysHospital :: LocationCard StMarysHospital
stMarysHospital = location
  StMarysHospital
  Cards.stMarysHospital
  2
  (PerPlayer 1)
  Plus
  [Diamond, Square]

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance HasAbilities env StMarysHospital where
  getAbilities iid window@(Window Timing.When NonFast) (StMarysHospital attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseAbilities iid window attrs $ pure [locationAbility (ability attrs)]
  getAbilities iid window (StMarysHospital attrs) =
    getAbilities iid window attrs

instance (LocationRunner env) => RunMessage env StMarysHospital where
  runMessage msg l@(StMarysHospital attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (HealDamage (InvestigatorTarget iid) 3)
    _ -> StMarysHospital <$> runMessage msg attrs
