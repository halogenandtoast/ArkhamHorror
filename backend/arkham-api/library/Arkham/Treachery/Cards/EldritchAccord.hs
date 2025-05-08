module Arkham.Treachery.Cards.EldritchAccord (eldritchAccord) where

import Arkham.Capability
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EldritchAccord = EldritchAccord TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchAccord :: TreacheryCard EldritchAccord
eldritchAccord = treachery EldritchAccord Cards.eldritchAccord

instance RunMessage EldritchAccord where
  runMessage msg t@(EldritchAccord attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      doStep 1 msg
      doStep 2 msg
      doStep 3 msg
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> withI18n do
      whenM (can.draw.cards iid) do
        chooseOneM iid do
          countVar 1 $ labeled' "drawCards" $ drawCards iid attrs 1
          labeled' "doNotDraw" nothing
      pure t
    DoStep 2 (Revelation iid (isSource attrs -> True)) -> withI18n do
      hand <- iid.hand
      focusCards hand do
        chooseUpToNM' iid 2 "doneDiscarding" do
          targets hand (discardCard iid attrs)
      pure t
    DoStep 3 (Revelation iid (isSource attrs -> True)) -> do
      hand <- iid.hand
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed $ length hand)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> EldritchAccord <$> liftRunMessage msg attrs
