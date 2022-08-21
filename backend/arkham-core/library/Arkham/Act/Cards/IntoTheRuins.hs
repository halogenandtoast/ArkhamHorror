module Arkham.Act.Cards.IntoTheRuins
  ( IntoTheRuins(..)
  , intoTheRuins
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Criteria
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Deck

newtype IntoTheRuins = IntoTheRuins ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheRuins :: ActCard IntoTheRuins
intoTheRuins = act (1, A) IntoTheRuins Cards.intoTheRuins Nothing

instance HasAbilities IntoTheRuins where
  getAbilities (IntoTheRuins a) =
    [ restrictedAbility a 1 (ScenarioDeckWithCard ExplorationDeck)
        $ ActionAbility (Just Action.Explore)
        $ ActionCost 1
    ]

instance RunMessage IntoTheRuins where
  runMessage msg a@(IntoTheRuins attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push $ Explore
        iid
        source
        (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    _ -> IntoTheRuins <$> runMessage msg attrs
