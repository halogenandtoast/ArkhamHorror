module Arkham.Treachery.Cards.ArkhamUnderAssault (arkhamUnderAssault) where

import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (drawCthulhuDeckCard)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArkhamUnderAssault = ArkhamUnderAssault TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamUnderAssault :: TreacheryCard ArkhamUnderAssault
arkhamUnderAssault = treachery ArkhamUnderAssault Cards.arkhamUnderAssault

instance RunMessage ArkhamUnderAssault where
  runMessage msg t@(ArkhamUnderAssault attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid attrs iid #willpower (ScenarioCount CthulhuRage)
      doStep 1 msg
      pure t
    FailedThisSkillTest _iid (isSource attrs -> True) -> do
      pure $ t & setMeta True
    DoStep 1 (Revelation _ (isSource attrs -> True)) -> do
      when (toResultDefault False attrs.meta) do
        lead <- getLead
        drawCthulhuDeckCard lead attrs
      pure t
    _ -> ArkhamUnderAssault <$> liftRunMessage msg attrs
