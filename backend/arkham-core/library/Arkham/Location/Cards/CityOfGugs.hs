module Arkham.Location.Cards.CityOfGugs (cityOfGugs, CityOfGugs (..)) where

import Arkham.GameValue
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype CityOfGugs = CityOfGugs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfGugs :: LocationCard CityOfGugs
cityOfGugs = location CityOfGugs Cards.cityOfGugs 2 (PerPlayer 1)

instance HasAbilities CityOfGugs where
  getAbilities (CityOfGugs a) =
    let restriction = if locationCanBeFlipped a then NoRestriction else Never
     in extendRevealed a [restrictedAbility a 1 restriction $ forced $ Enters #after You $ be a]

instance RunMessage CityOfGugs where
  runMessage msg l@(CityOfGugs attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOver iid attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theSentry
      pure . CityOfGugs $ attrs & canBeFlippedL .~ False
    _ -> CityOfGugs <$> lift (runMessage msg attrs)
