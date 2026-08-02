module Arkham.Treachery.Cards.Acrophobia (acrophobia) where

import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.ObsidianCanyons.Helpers (isOpenSky)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Acrophobia = Acrophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acrophobia :: TreacheryCard Acrophobia
acrophobia = treachery Acrophobia Cards.acrophobia

instance RunMessage Acrophobia where
  runMessage msg t@(Acrophobia attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 1, CountLocations (connectedFrom (locationWithInvestigator iid) <> isOpenSky)]
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n (FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      x <- field InvestigatorRemainingActions iid
      chooseOrRunOneM iid $ withI18n do
        when (x > 0) $ chooseLoseActions iid attrs 1
        chooseTakeHorror iid attrs 1
      doNextStep msg
      pure t
    _ -> Acrophobia <$> liftRunMessage msg attrs
