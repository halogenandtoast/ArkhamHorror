module Arkham.Event.Events.ExtraAmmunition1 where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype ExtraAmmunition1 = ExtraAmmunition1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extraAmmunition1 :: EventCard ExtraAmmunition1
extraAmmunition1 = event ExtraAmmunition1 Cards.extraAmmunition1

instance RunMessage ExtraAmmunition1 where
  runMessage msg e@(ExtraAmmunition1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      investigatorTargets <- guardAffectsColocated iid
      firearms <- select $ #firearm <> AssetControlledBy investigatorTargets
      chooseOrRunOneM iid $ targets firearms $ addUsesOn attrs #ammo 3
      pure e
    _ -> ExtraAmmunition1 <$> liftRunMessage msg attrs
