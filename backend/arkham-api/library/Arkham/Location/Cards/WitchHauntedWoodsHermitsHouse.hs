module Arkham.Location.Cards.WitchHauntedWoodsHermitsHouse (witchHauntedWoodsHermitsHouse) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype WitchHauntedWoodsHermitsHouse = WitchHauntedWoodsHermitsHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsHermitsHouse :: LocationCard WitchHauntedWoodsHermitsHouse
witchHauntedWoodsHermitsHouse = location WitchHauntedWoodsHermitsHouse Cards.witchHauntedWoodsHermitsHouse 4 (PerPlayer 2)

instance HasAbilities WitchHauntedWoodsHermitsHouse where
  getAbilities (WitchHauntedWoodsHermitsHouse a) =
    withBaseAbilities
      a
      [ restricted a 1 (exists $ investigatorAt $ toId a)
          $ forced
          $ DiscoverClues #after You (be a) (atLeast 1)
      ]

getCount :: [Window] -> Int
getCount [] = error "wrong window"
getCount ((windowType -> Window.DiscoverClues _ _ _ n) : _) = n
getCount (_ : xs) = getCount xs

instance RunMessage WitchHauntedWoodsHermitsHouse where
  runMessage msg l@(WitchHauntedWoodsHermitsHouse attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getCount -> n) _ -> do
      iids <- select $ investigatorAt $ toId attrs
      pushAll [toMessage $ randomDiscardN iid attrs n | iid <- iids]
      pure l
    _ -> WitchHauntedWoodsHermitsHouse <$> runMessage msg attrs
