module Arkham.Location.Cards.WashroomHemlockHouse37 (washroomHemlockHouse37) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid (Pos (..))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))

newtype WashroomHemlockHouse37 = WashroomHemlockHouse37 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

washroomHemlockHouse37 :: LocationCard WashroomHemlockHouse37
washroomHemlockHouse37 =
  locationWith WashroomHemlockHouse37 Cards.washroomHemlockHouse37 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WashroomHemlockHouse37 where
  getAbilities (WashroomHemlockHouse37 a) =
    extendRevealed1 a
      $ groupLimit PerTurn
      $ restricted a 1 (Here <> CluesOnThis (atLeast 1) <> exists below)
      $ FastAbility Free
   where
    -- "the location directly below it" = Pos (col, y-1) on the grid
    below = case a.position of
      Just (Pos x y) -> LocationInPosition (Pos x (y - 1))
      Nothing -> Nowhere

instance RunMessage WashroomHemlockHouse37 where
  runMessage msg l@(WashroomHemlockHouse37 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      for_ attrs.position \(Pos x y) -> do
        mBelow <- selectOne (LocationInPosition (Pos x (y - 1)))
        for_ mBelow \below -> do
          placeTokens (attrs.ability 1) below Clue 1
          removeTokens (attrs.ability 1) attrs Clue 1
      pure l
    _ -> WashroomHemlockHouse37 <$> liftRunMessage msg attrs
