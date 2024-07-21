module Arkham.Event.Cards.HonedInstinct (honedInstinct, HonedInstinct (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Customization
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Matcher

newtype HonedInstinct = HonedInstinct EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

honedInstinct :: EventCard HonedInstinct
honedInstinct = event HonedInstinct Cards.honedInstinct

instance HasModifiersFor HonedInstinct where
  getModifiersFor target (HonedInstinct a) | isTarget a target = do
    modified a $ guard (a `hasCustomization` ImpulseControl) $> ReduceCostOf (CardWithId a.cardId) 1
  getModifiersFor _ _ = pure []

instance RunMessage HonedInstinct where
  runMessage msg e@(HonedInstinct attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      when (attrs `hasCustomization` SharpenedTalent) do
        eventModifiers attrs iid [SkillModifier sType 2 | sType <- [minBound ..]]
      pushAll [GainActions iid (toSource attrs) 1, PlayerWindow iid [] True, DoStep 1 msg]
      pure e
    DoStep 1 (PlayThisEvent iid (is attrs -> True)) -> do
      when (attrs `hasCustomization` ForceOfHabit) do
        chooseOne
          iid
          [ Label
              "Perform another action and remove this from game  (Force of Habit)"
              [GainActions iid (toSource attrs) 1, PlayerWindow iid [] True, RemoveFromGame (toTarget attrs)]
          , Label "Do not perform another action" []
          ]
      pure e
    _ -> HonedInstinct <$> liftRunMessage msg attrs
