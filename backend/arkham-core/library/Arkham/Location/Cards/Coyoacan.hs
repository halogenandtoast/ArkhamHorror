module Arkham.Location.Cards.Coyoacan
  ( coyoacan
  , Coyoacan(..)
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
import Arkham.Scenario.Deck
import Arkham.Target

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
  runMessage msg a@(Coyoacan attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) _ 1 _ -> do
      push
        $ Explore iid (toSource attrs)
        $ CardWithPrintedLocationSymbol
        $ locationSymbol attrs
      pure a
    _ -> Coyoacan <$> runMessage msg attrs
