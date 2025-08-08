module Arkham.Event.Events.HonedInstinct (honedInstinct) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Customization
import Arkham.Helpers.Modifiers hiding (eventModifiers)
import Arkham.Matcher

newtype HonedInstinct = HonedInstinct EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

honedInstinct :: EventCard HonedInstinct
honedInstinct = event HonedInstinct Cards.honedInstinct

instance HasModifiersFor HonedInstinct where
  getModifiersFor (HonedInstinct a) =
    modifySelfWhen a.cardId (a `hasCustomization` ImpulseControl) [ReduceCostOf (CardWithId a.cardId) 1]

instance RunMessage HonedInstinct where
  runMessage msg e@(HonedInstinct attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      when (attrs `hasCustomization` SharpenedTalent) do
        eventModifiers attrs iid [SkillModifier sType 2 | sType <- [minBound ..]]
      takeActionAsIfTurn iid attrs
      doStep 1 msg
      pure e
    DoStep 1 (PlayThisEvent iid (is attrs -> True)) -> do
      when (attrs `hasCustomization` ForceOfHabit) do
        chooseOneM iid do
          labeled "Perform another action and remove this from game  (Force of Habit)" do
            takeActionAsIfTurn iid attrs
          labeled "Do not perform another action" nothing
      pure e
    _ -> HonedInstinct <$> liftRunMessage msg attrs
