module Arkham.Event.Events.EmergencyAid (emergencyAid) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype EmergencyAid = EmergencyAid EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyAid :: EventCard EmergencyAid
emergencyAid = event EmergencyAid Cards.emergencyAid

instance RunMessage EmergencyAid where
  runMessage msg e@(EmergencyAid attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      iids <- select =<< guardAffectsColocated iid

      chooseOneM iid do
        for_ iids \iid' -> do
          healableAllies <-
            select $ HealableAsset (toSource attrs) #damage $ AllyAsset <> assetControlledBy iid'

          targeting iid' do
            chooseOrRunOneM iid do
              whenM (canHaveDamageHealed attrs iid') $ targeting iid' $ healDamage iid' attrs 2
              for_ healableAllies \asset ->
                targeting asset $ healDamage asset attrs 2
      pure e
    _ -> EmergencyAid <$> liftRunMessage msg attrs
