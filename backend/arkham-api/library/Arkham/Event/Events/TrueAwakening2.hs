module Arkham.Event.Events.TrueAwakening2 (trueAwakening2) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator

newtype TrueAwakening2 = TrueAwakening2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueAwakening2 :: EventCard TrueAwakening2
trueAwakening2 = event TrueAwakening2 Cards.trueAwakening2

instance RunMessage TrueAwakening2 where
  runMessage msg e@(TrueAwakening2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      chooseOneM iid do
        whenM (can.draw.cards iid) $ labeled "Draw 2 cards" do
          drawCards iid attrs 2
          doStep 1 msg
        whenM (canDiscoverCluesAtYourLocation NotInvestigate iid) do
          labeled "Discover 1 clue at your location" do
            discoverAtYourLocation NotInvestigate iid attrs 1
            doStep 2 msg
        whenM (canHaveDamageHealed attrs iid) $ labeled "Heal 1 damage" do
          healDamage iid attrs 1
          doStep 3 msg
        whenM (canHaveHorrorHealed attrs iid) $ labeled "Heal 1 horror" do
          healHorror iid attrs 1
          doStep 4 msg
      pure e
    DoStep n (PlayThisEvent iid (is attrs -> True)) -> do
      chooseOneM iid do
        when (n /= 1) do
          whenM (can.draw.cards iid) $ labeled "Draw 2 cards" $ drawCards iid attrs 2
        when (n /= 2) do
          whenM (canDiscoverCluesAtYourLocation NotInvestigate iid) do
            labeled "Discover 1 clue at your location" $ discoverAtYourLocation NotInvestigate iid attrs 1
        when (n /= 3) do
          whenM (canHaveDamageHealed attrs iid) $ labeled "Heal 1 damage" $ healDamage iid attrs 1
        when (n /= 4) do
          whenM (canHaveHorrorHealed attrs iid) $ labeled "Heal 1 horror" $ healHorror iid attrs 1
      pure e
    _ -> TrueAwakening2 <$> liftRunMessage msg attrs
