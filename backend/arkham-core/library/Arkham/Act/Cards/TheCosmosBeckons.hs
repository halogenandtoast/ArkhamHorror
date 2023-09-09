module Arkham.Act.Cards.TheCosmosBeckons (
  TheCosmosBeckons (..),
  theCosmosBeckons,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Helpers.Ability
import Arkham.Matcher

newtype TheCosmosBeckons = TheCosmosBeckons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCosmosBeckons :: ActCard TheCosmosBeckons
theCosmosBeckons =
  act
    (1, A)
    TheCosmosBeckons
    Cards.theCosmosBeckons
    (Just $ GroupClueCost (PerPlayer 1) "Hideous Palace")

instance HasAbilities TheCosmosBeckons where
  getAbilities (TheCosmosBeckons attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            (InvestigatorExists $ You <> InvestigatorCanSpendClues)
            $ ActionAbility Nothing
            $ ActionCost 1
        ]

instance RunMessage TheCosmosBeckons where
  runMessage msg (TheCosmosBeckons attrs) = TheCosmosBeckons <$> runMessage msg attrs
