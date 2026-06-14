module Arkham.Treachery.Cards.Acrophobia (acrophobia) where

import Arkham.I18n
import Arkham.Message.Lifted.Choose
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
      -- TODO: this test gets +1 difficulty for each "open sky" card adjacent to
      -- your location. "Open sky" placeholder cards / the Summit deck have no
      -- engine support yet, so the difficulty is fixed at the base value of 1.
      revelationSkillTest sid iid attrs #willpower (Fixed 1)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      -- For each point you fail by, you must either lose 1 action or take 1
      -- horror (a separate choice per point of failure).
      replicateM_ n $ chooseOneM iid $ withI18n do
        chooseLoseActions iid attrs 1
        chooseTakeHorror iid attrs 1
      pure t
    _ -> Acrophobia <$> liftRunMessage msg attrs
