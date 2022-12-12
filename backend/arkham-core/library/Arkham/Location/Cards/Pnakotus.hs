module Arkham.Location.Cards.Pnakotus
  ( pnakotus
  , Pnakotus(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Message

newtype Pnakotus = Pnakotus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pnakotus :: LocationCard Pnakotus
pnakotus = location Pnakotus Cards.pnakotus 2 (Static 3)

instance HasAbilities Pnakotus where
  getAbilities (Pnakotus a) = withBaseAbilities
    a
    [restrictedAbility a 1 Here $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage Pnakotus where
  runMessage msg l@(Pnakotus attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      pure l
    _ -> Pnakotus <$> runMessage msg attrs
