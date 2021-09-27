module Arkham.Types.Location.Cards.OvergrownCairns
  ( OvergrownCairns(..)
  , overgrownCairns
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (overgrownCairns)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Target

newtype OvergrownCairns = OvergrownCairns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overgrownCairns :: LocationCard OvergrownCairns
overgrownCairns = location
  OvergrownCairns
  Cards.overgrownCairns
  4
  (Static 0)
  Equals
  [Hourglass, Equals]

instance HasAbilities OvergrownCairns where
  getAbilities (OvergrownCairns attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility
            attrs
            1
            Here
            (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 2])
          & (abilityLimitL .~ PlayerLimit PerGame 1)
      | locationRevealed attrs
      ]

instance (LocationRunner env) => RunMessage env OvergrownCairns where
  runMessage msg l@(OvergrownCairns attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ pushAll [HealHorror (InvestigatorTarget iid) 2]
    _ -> OvergrownCairns <$> runMessage msg attrs
