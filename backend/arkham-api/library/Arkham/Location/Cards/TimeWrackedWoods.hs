module Arkham.Location.Cards.TimeWrackedWoods (timeWrackedWoods) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait

newtype TimeWrackedWoods = TimeWrackedWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeWrackedWoods :: LocationCard TimeWrackedWoods
timeWrackedWoods =
  symbolLabel $ location TimeWrackedWoods Cards.timeWrackedWoods 4 (PerPlayer 2)

instance HasAbilities TimeWrackedWoods where
  getAbilities (TimeWrackedWoods attrs) =
    extendRevealed1 attrs
      $ groupLimit PerGame
      $ restricted
        attrs
        1
        (Here <> InVictoryDisplay (CardWithVengeance <> not_ (CardWithTrait Elite)) (atLeast 1))
      $ ActionAbility []
      $ ActionCost 2

instance RunMessage TimeWrackedWoods where
  runMessage msg l@(TimeWrackedWoods attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      xs <-
        onlyEncounterCards
          <$> select (VictoryDisplayCardMatch $ basic $ CardWithVengeance <> not_ (CardWithTrait Elite))
      focusCards xs $ chooseOrRunOneM iid $ targets xs (push . AddToEncounterDiscard)
      pure l
    _ -> TimeWrackedWoods <$> liftRunMessage msg attrs
