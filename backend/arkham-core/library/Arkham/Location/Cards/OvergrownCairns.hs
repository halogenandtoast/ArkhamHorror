module Arkham.Location.Cards.OvergrownCairns
  ( OvergrownCairns(..)
  , overgrownCairns
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (overgrownCairns)
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Message
import Arkham.Target

newtype OvergrownCairns = OvergrownCairns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
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

instance RunMessage OvergrownCairns where
  runMessage msg l@(OvergrownCairns attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ pushAll [HealHorror (InvestigatorTarget iid) 2]
    _ -> OvergrownCairns <$> runMessage msg attrs
