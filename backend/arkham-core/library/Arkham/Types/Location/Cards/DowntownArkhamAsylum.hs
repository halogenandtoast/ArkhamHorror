module Arkham.Types.Location.Cards.DowntownArkhamAsylum
  ( DowntownArkhamAsylum(..)
  , downtownArkhamAsylum
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (downtownArkhamAsylum)
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

newtype DowntownArkhamAsylum = DowntownArkhamAsylum LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownArkhamAsylum :: LocationId -> DowntownArkhamAsylum
downtownArkhamAsylum = DowntownArkhamAsylum . baseAttrs
  Cards.downtownArkhamAsylum
  4
  (PerPlayer 2)
  Triangle
  [Moon, T]

instance HasModifiersFor env DowntownArkhamAsylum where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance ActionRunner env => HasActions env DowntownArkhamAsylum where
  getActions iid NonFast (DowntownArkhamAsylum attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `elem` locationInvestigators
      ]
  getActions iid window (DowntownArkhamAsylum attrs) =
    getActions iid window attrs

instance (LocationRunner env) => RunMessage env DowntownArkhamAsylum where
  runMessage msg l@(DowntownArkhamAsylum attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage (HealHorror (InvestigatorTarget iid) 3)
    _ -> DowntownArkhamAsylum <$> runMessage msg attrs
