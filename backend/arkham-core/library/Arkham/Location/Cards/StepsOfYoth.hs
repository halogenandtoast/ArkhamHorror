module Arkham.Location.Cards.StepsOfYoth
  ( stepsOfYoth
  , StepsOfYoth(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype StepsOfYoth = StepsOfYoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfYoth :: LocationCard StepsOfYoth
stepsOfYoth = symbolLabel $ location StepsOfYoth Cards.stepsOfYoth 3 (Static 0)

instance HasAbilities StepsOfYoth where
  getAbilities (StepsOfYoth attrs) = withBaseAbilities
    attrs
    [ limitedAbility (GroupLimit PerGame 1)
      $ restrictedAbility attrs 1 Here
      $ ReactionAbility AddingToCurrentDepth
      $ SupplyCost (LocationWithId $ toId attrs) Rope
    ]

instance RunMessage StepsOfYoth where
  runMessage msg l@(StepsOfYoth attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      msgs <- incrementDepth
      pushAll msgs
      pure l
    _ -> StepsOfYoth <$> runMessage msg attrs
