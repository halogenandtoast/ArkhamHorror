module Arkham.Location.Cards.OvergrownCairns
  ( OvergrownCairns(..)
  , overgrownCairns
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Damage
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( overgrownCairns )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype OvergrownCairns = OvergrownCairns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overgrownCairns :: LocationCard OvergrownCairns
overgrownCairns = location OvergrownCairns Cards.overgrownCairns 4 (Static 0)

instance HasAbilities OvergrownCairns where
  getAbilities (OvergrownCairns attrs) =
    withBaseAbilities attrs
      $ [ limitedAbility (PlayerLimit PerGame 1)
          $ restrictedAbility
              attrs
              1
              (Here <> InvestigatorExists
                (HealableInvestigator (toSource attrs) HorrorType You)
              )
          $ ActionAbility Nothing
          $ Costs [ActionCost 1, ResourceCost 2]
        | locationRevealed attrs
        ]

instance RunMessage OvergrownCairns where
  runMessage msg l@(OvergrownCairns attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ pushAll [HealHorror (InvestigatorTarget iid) (toSource attrs) 2]
    _ -> OvergrownCairns <$> runMessage msg attrs
