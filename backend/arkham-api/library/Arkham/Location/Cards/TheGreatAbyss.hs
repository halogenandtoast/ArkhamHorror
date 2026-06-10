module Arkham.Location.Cards.TheGreatAbyss (theGreatAbyss) where

import Arkham.Ability
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype TheGreatAbyss = TheGreatAbyss LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatAbyss :: LocationCard TheGreatAbyss
theGreatAbyss = locationWith TheGreatAbyss Cards.theGreatAbyss 5 (Static 1) (canBeFlippedL .~ True)

instance HasAbilities TheGreatAbyss where
  getAbilities (TheGreatAbyss a) =
    extendRevealed1 a
      $ restricted a 1 (thisExists a LocationCanBeFlipped)
      $ freeReaction
      $ DiscoveringLastClue #after You (be a)

instance RunMessage TheGreatAbyss where
  runMessage msg l@(TheGreatAbyss attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Stories.fateOfTheDreamers
      pure . TheGreatAbyss $ attrs & canBeFlippedL .~ False
    _ -> TheGreatAbyss <$> liftRunMessage msg attrs
