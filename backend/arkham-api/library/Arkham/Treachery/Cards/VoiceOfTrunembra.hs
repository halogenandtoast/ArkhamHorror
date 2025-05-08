module Arkham.Treachery.Cards.VoiceOfTrunembra (voiceOfTrunembra) where

import Arkham.Effect.Builder
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VoiceOfTrunembra = VoiceOfTrunembra TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

voiceOfTrunembra :: TreacheryCard VoiceOfTrunembra
voiceOfTrunembra = treachery VoiceOfTrunembra Cards.voiceOfTrunembra

instance RunMessage VoiceOfTrunembra where
  runMessage msg t@(VoiceOfTrunembra attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      chooseNM iid (min 3 n) $ withI18n do
        countVar 2 $ labeled' "loseResources" $ loseResources iid attrs 2
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
        countVar 2 $ labeled' "minusToYourNextSkillTestThisRound" $ doStep 3 msg
      pure t
    DoStep 3 (FailedThisSkillTestBy iid (isSource attrs -> True) _) -> do
      -- we have to delay calling this until outside of the question in order
      -- to affect the queue correctly
      afterSkillTestQuiet $ withSource attrs $ effect iid do
        during #nextSkillTest
        removeOn #round
        apply $ AnySkillValue (-2)
      pure t
    _ -> VoiceOfTrunembra <$> liftRunMessage msg attrs
