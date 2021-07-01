module Arkham.Types.Location.Cards.OvergrownCairns
  ( OvergrownCairns(..)
  , overgrownCairns
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (overgrownCairns)
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

newtype OvergrownCairns = OvergrownCairns LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overgrownCairns :: LocationId -> OvergrownCairns
overgrownCairns = OvergrownCairns . baseAttrs
  Cards.overgrownCairns
  4
  (Static 0)
  Equals
  [Hourglass, Equals]

instance HasModifiersFor env OvergrownCairns where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs = base { abilityLimit = PlayerLimit PerGame 1 }
 where
  base = mkAbility
    (toSource attrs)
    1
    (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 2])

instance ActionRunner env => HasActions env OvergrownCairns where
  getActions iid NonFast (OvergrownCairns attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `member` locationInvestigators
      ]
  getActions i window (OvergrownCairns attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env OvergrownCairns where
  runMessage msg l@(OvergrownCairns attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessages [HealHorror (InvestigatorTarget iid) 2]
    _ -> OvergrownCairns <$> runMessage msg attrs
