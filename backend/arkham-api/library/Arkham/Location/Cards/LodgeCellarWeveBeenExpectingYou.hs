module Arkham.Location.Cards.LodgeCellarWeveBeenExpectingYou (lodgeCellarWeveBeenExpectingYou) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ForTheGreaterGood.Helpers

newtype LodgeCellarWeveBeenExpectingYou = LodgeCellarWeveBeenExpectingYou LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeCellarWeveBeenExpectingYou :: LocationCard LodgeCellarWeveBeenExpectingYou
lodgeCellarWeveBeenExpectingYou =
  location LodgeCellarWeveBeenExpectingYou Cards.lodgeCellarWeveBeenExpectingYou 3 (PerPlayer 1)

instance HasAbilities LodgeCellarWeveBeenExpectingYou where
  getAbilities (LodgeCellarWeveBeenExpectingYou a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage LodgeCellarWeveBeenExpectingYou where
  runMessage msg l@(LodgeCellarWeveBeenExpectingYou attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      getRandomKey >>= traverse_ (placeKey attrs)
      pure l
    _ -> LodgeCellarWeveBeenExpectingYou <$> liftRunMessage msg attrs
