module Arkham.Treachery.Cards.GroundDisturbance (groundDisturbance) where

import Arkham.Helpers.SkillTest.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheThingInTheDepths.Helpers
import Arkham.SkillTest.Base
import Arkham.Trait (Trait (Bog))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GroundDisturbance = GroundDisturbance TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

groundDisturbance :: TreacheryCard GroundDisturbance
groundDisturbance = treachery GroundDisturbance Cards.groundDisturbance

instance RunMessage GroundDisturbance where
  runMessage msg t@(GroundDisturbance attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseBeginSkillTestEdit sid iid attrs iid [#intellect, #agility] (Fixed 4) setIsRevelation
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n | n > 0 -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      bogs <- select $ NearestLocationTo iid (LocationWithTrait Bog)
      chooseOneM iid $ scenarioI18n do
        unscoped $ countVar 1 $ labeled' "takeDamage" do
          assignDamage iid attrs 1
          doStep (n - 1) msg'
        when (notNull bogs) do
          labeled' "groundDisturbance.placeSinkhole" do
            chooseOrRunOneM iid do
              targets bogs \lid -> placeTokens attrs lid #damage 1
            doStep (n - 1) msg'
      pure t
    _ -> GroundDisturbance <$> liftRunMessage msg attrs
