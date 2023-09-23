module Arkham.Location.Cards.WitchHauntedWoodsCairnStones (
  witchHauntedWoodsCairnStones,
  WitchHauntedWoodsCairnStones (..),
) where

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

newtype WitchHauntedWoodsCairnStones = WitchHauntedWoodsCairnStones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsCairnStones :: LocationCard WitchHauntedWoodsCairnStones
witchHauntedWoodsCairnStones =
  location
    WitchHauntedWoodsCairnStones
    Cards.witchHauntedWoodsCairnStones
    3
    (PerPlayer 2)

instance HasAbilities WitchHauntedWoodsCairnStones where
  getAbilities (WitchHauntedWoodsCairnStones a) =
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

instance RunMessage WitchHauntedWoodsCairnStones where
  runMessage msg l@(WitchHauntedWoodsCairnStones attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getCount -> n) _ -> do
      iids <- selectList $ investigatorAt $ toId attrs
      pushAll [LoseResources iid (toSource attrs) n | iid <- iids]
      pure l
    _ -> WitchHauntedWoodsCairnStones <$> runMessage msg attrs
