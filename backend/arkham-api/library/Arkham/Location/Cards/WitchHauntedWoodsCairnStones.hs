module Arkham.Location.Cards.WitchHauntedWoodsCairnStones (witchHauntedWoodsCairnStones) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype WitchHauntedWoodsCairnStones = WitchHauntedWoodsCairnStones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsCairnStones :: LocationCard WitchHauntedWoodsCairnStones
witchHauntedWoodsCairnStones =
  location WitchHauntedWoodsCairnStones Cards.witchHauntedWoodsCairnStones 3 (PerPlayer 2)

instance HasAbilities WitchHauntedWoodsCairnStones where
  getAbilities (WitchHauntedWoodsCairnStones a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ investigatorAt $ toId a)
      $ forced
      $ DiscoverClues #after You (be a) (atLeast 1)

getCount :: [Window] -> Int
getCount [] = error "wrong window"
getCount ((windowType -> Window.DiscoverClues _ _ _ n) : _) = n
getCount (_ : xs) = getCount xs

instance RunMessage WitchHauntedWoodsCairnStones where
  runMessage msg l@(WitchHauntedWoodsCairnStones attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getCount -> n) _ -> do
      iids <- select $ investigatorAt $ toId attrs
      for_ iids \iid -> loseResources iid attrs n
      pure l
    _ -> WitchHauntedWoodsCairnStones <$> liftRunMessage msg attrs
