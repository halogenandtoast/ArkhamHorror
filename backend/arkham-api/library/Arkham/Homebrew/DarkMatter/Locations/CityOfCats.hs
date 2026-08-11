module Arkham.Homebrew.DarkMatter.Locations.CityOfCats (cityOfCats) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CityOfCats = CityOfCats LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfCats :: LocationCard CityOfCats
cityOfCats = location CityOfCats Cards.cityOfCats 3 (PerPlayer 1)

{- | "[action] Parley. Discard 1 card of each cardtype (asset, event, and skill)
from your hand: Heal 1 mental trauma. (Group limit once per game.)"
-}
instance HasAbilities CityOfCats where
  getAbilities (CityOfCats a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ parleyAction
      $ HandDiscardCost 1 (basic #asset)
      <> HandDiscardCost 1 (basic #event)
      <> HandDiscardCost 1 (basic #skill)

instance RunMessage CityOfCats where
  runMessage msg l@(CityOfCats attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ HealTrauma iid 0 1
      pure l
    _ -> CityOfCats <$> liftRunMessage msg attrs
