module Arkham.Location.Cards.StepsOfYoth
  ( stepsOfYoth
  , StepsOfYoth(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype StepsOfYoth = StepsOfYoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfYoth :: LocationCard StepsOfYoth
stepsOfYoth = symbolLabel $ location StepsOfYoth Cards.stepsOfYoth 3 (Static 0)

instance HasAbilities StepsOfYoth where
  getAbilities (StepsOfYoth attrs) = withBaseAbilities
    attrs
    [ limitedAbility (GroupLimit PerGame 1) $ restrictedAbility a 1 Here
      $ ReactionAbility AddingToCurrentDepth
      $ SupplyCost (LocationWithId $ toId attrs) Rope
    ]

instance RunMessage StepsOfYoth where
  runMessage msg l@(StepsOfYoth attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      msgs <- incrementDepth
      push msgs
      pure l
    _ -> StepsOfYoth <$> runMessage msg attrs
