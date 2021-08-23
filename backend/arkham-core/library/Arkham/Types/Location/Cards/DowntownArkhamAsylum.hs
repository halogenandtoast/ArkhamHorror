module Arkham.Types.Location.Cards.DowntownArkhamAsylum
  ( DowntownArkhamAsylum(..)
  , downtownArkhamAsylum
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (downtownArkhamAsylum)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype DowntownArkhamAsylum = DowntownArkhamAsylum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownArkhamAsylum :: LocationCard DowntownArkhamAsylum
downtownArkhamAsylum = location
  DowntownArkhamAsylum
  Cards.downtownArkhamAsylum
  4
  (PerPlayer 2)
  Triangle
  [Moon, T]

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance HasAbilities env DowntownArkhamAsylum where
  getAbilities iid window@(Window Timing.When NonFast) (DowntownArkhamAsylum attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseAbilities iid window attrs $ pure [locationAbility (ability attrs)]
  getAbilities iid window (DowntownArkhamAsylum attrs) =
    getAbilities iid window attrs

instance (LocationRunner env) => RunMessage env DowntownArkhamAsylum where
  runMessage msg l@(DowntownArkhamAsylum attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (HealHorror (InvestigatorTarget iid) 3)
    _ -> DowntownArkhamAsylum <$> runMessage msg attrs
