module Arkham.Treachery.Cards.Downpour_c2026 (downpour_c2026) where

import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Downpour_c2026 = Downpour_c2026 TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downpour_c2026 :: TreacheryCard Downpour_c2026
downpour_c2026 = treachery Downpour_c2026 Cards.downpour_c2026

instance RunMessage Downpour_c2026 where
  runMessage msg t@(Downpour_c2026 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTestBy iid (isSource attrs -> True) _) -> do
      actions <- field InvestigatorRemainingActions iid
      clues <- field InvestigatorClues iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeledValidate' (actions > 0) "loseActions" do
          loseActions iid attrs 1
          doStep (n - 1) msg'
        countVar 1 $ labeledValidate' (clues > 0) "placeCluesOnYourLocation" do
          placeCluesOnLocation iid attrs 1
          doStep (n - 1) msg'
      pure t
    _ -> Downpour_c2026 <$> liftRunMessage msg attrs
