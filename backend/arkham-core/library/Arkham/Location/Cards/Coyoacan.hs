module Arkham.Location.Cards.Coyoacan
  ( coyoacan
  , Coyoacan(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype Coyoacan = Coyoacan LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coyoacan :: LocationCard Coyoacan
coyoacan = locationWith Coyoacan Cards.coyoacan 2 (Static 0) (labelL .~ "star")

instance HasAbilities Coyoacan where
  getAbilities (Coyoacan attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (Here <> ScenarioDeckWithCard ExplorationDeck)
      $ ActionAbility (Just Action.Explore)
      $ ActionCost 1
      <> OrCost
           [ DamageCost (toSource attrs) YouTarget 1
           , HorrorCost (toSource attrs) YouTarget 1
           ]
    | locationRevealed attrs
    ]

instance RunMessage Coyoacan where
  runMessage msg l@(Coyoacan attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ Explore iid (toSource attrs)
        $ CardWithPrintedLocationSymbol
        $ locationSymbol attrs
      pure l
    _ -> Coyoacan <$> runMessage msg attrs
