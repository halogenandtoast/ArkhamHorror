module Arkham.Location.Cards.TempleRuins
  ( templeRuins
  , TempleRuins(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message

newtype TempleRuins = TempleRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeRuins :: LocationCard TempleRuins
templeRuins =
  locationWith TempleRuins Cards.templeRuins 4 (Static 0) (labelL .~ "circle")

instance HasAbilities TempleRuins where
  getAbilities (TempleRuins attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here
      $ ActionAbility (Just Action.Explore)
      $ ActionCost 2
    | locationRevealed attrs
    ]

instance RunMessage TempleRuins where
  runMessage msg l@(TempleRuins attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) _ 1 _ -> do
      push
        $ Explore iid (toSource attrs)
        $ CardWithPrintedLocationSymbol
        $ locationSymbol attrs
      pure l
    _ -> TempleRuins <$> runMessage msg attrs
