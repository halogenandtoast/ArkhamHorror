module Arkham.Event.Events.TrueAwakening2 (trueAwakening2) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.I18n

newtype TrueAwakening2 = TrueAwakening2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueAwakening2 :: EventCard TrueAwakening2
trueAwakening2 = event TrueAwakening2 Cards.trueAwakening2

instance RunMessage TrueAwakening2 where
  runMessage msg e@(TrueAwakening2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      chooseOneM iid do
        whenM (can.draw.cards iid) $ withI18n $ countVar 2 $ labeledI18n "drawCards" do
          drawCards iid attrs 2
          doStep 1 msg
        whenM (canDiscoverCluesAtYourLocation NotInvestigate iid) $ withI18n $ countVar 1 do
          labeledI18n "discoverAtYourLocation" do
            discoverAtYourLocation NotInvestigate iid attrs 1
            doStep 2 msg
        whenM (canHaveDamageHealed attrs iid) $ withI18n $ countVar 1 $ labeledI18n "healDamage" do
          healDamage iid attrs 1
          doStep 3 msg
        whenM (canHaveHorrorHealed attrs iid) $ withI18n $ countVar 1 $ labeledI18n "healHorror" do
          healHorror iid attrs 1
          doStep 4 msg
      pure e
    DoStep n (PlayThisEvent iid (is attrs -> True)) -> do
      chooseOneM iid do
        when (n /= 1) do
          whenM (can.draw.cards iid) $ withI18n $ countVar 2 $ labeledI18n "drawCards" $ drawCards iid attrs 2
        when (n /= 2) do
          whenM (canDiscoverCluesAtYourLocation NotInvestigate iid) $ withI18n $ countVar 1 do
            labeledI18n "discoverAtYourLocation" $ discoverAtYourLocation NotInvestigate iid attrs 1
        when (n /= 3) do
          whenM (canHaveDamageHealed attrs iid) $ withI18n $ countVar 1 $ labeledI18n "healDamage" $ healDamage iid attrs 1
        when (n /= 4) do
          whenM (canHaveHorrorHealed attrs iid) $ withI18n $ countVar 1 $ labeledI18n "healHorror" $ healHorror iid attrs 1
      pure e
    _ -> TrueAwakening2 <$> liftRunMessage msg attrs
