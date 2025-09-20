module Arkham.Treachery.Cards.Bathophobia (bathophobia) where

import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Bathophobia = Bathophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bathophobia :: TreacheryCard Bathophobia
bathophobia = treachery Bathophobia Cards.bathophobia

instance RunMessage Bathophobia where
  runMessage msg t@(Bathophobia attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 1, ScenarioCount CurrentDepth]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> Bathophobia <$> liftRunMessage msg attrs
