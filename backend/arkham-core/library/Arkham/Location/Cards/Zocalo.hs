module Arkham.Location.Cards.Zocalo
  ( zocalo
  , Zocalo(..)
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

newtype Zocalo = Zocalo LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zocalo :: LocationCard Zocalo
zocalo = locationWith Zocalo Cards.zocalo 3 (Static 0) (labelL .~ "diamond")

instance HasAbilities Zocalo where
  getAbilities (Zocalo attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here
      $ ActionAbility (Just Action.Explore)
      $ ActionCost 1
      <> DiscardCombinedCost 5
    | locationRevealed attrs
    ]

instance RunMessage Zocalo where
  runMessage msg l@(Zocalo attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) _ 1 _ -> do
      push
        $ Explore iid (toSource attrs)
        $ CardWithPrintedLocationSymbol
        $ locationSymbol attrs
      pure l
    _ -> Zocalo <$> runMessage msg attrs
