module Arkham.Location.Cards.BoxOffice (boxOffice) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype BoxOffice = BoxOffice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boxOffice :: LocationCard BoxOffice
boxOffice = location BoxOffice Cards.boxOffice 2 (Static 0)

instance HasAbilities BoxOffice where
  getAbilities (BoxOffice a) = extendRevealed1 a $ groupLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage BoxOffice where
  runMessage msg l@(BoxOffice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 5
      remember StoleFromTheBoxOffice
      pure l
    _ -> BoxOffice <$> liftRunMessage msg attrs
