module Arkham.Location.Cards.WitchHauntedWoodsHermitsHouse (
  witchHauntedWoodsHermitsHouse,
  WitchHauntedWoodsHermitsHouse (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
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
      [ restrictedAbility a 1 (InvestigatorExists $ investigatorAt $ toId a)
          $ ForcedAbility
          $ DiscoverClues
            Timing.After
            You
            (LocationWithId $ toId a)
            (AtLeast $ Static 1)
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
