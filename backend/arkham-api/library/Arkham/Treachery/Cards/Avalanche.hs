module Arkham.Treachery.Cards.Avalanche (avalanche) where

import Arkham.Helpers.Investigator
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Avalanche = Avalanche TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

avalanche :: TreacheryCard Avalanche
avalanche = treachery Avalanche Cards.avalanche

instance RunMessage Avalanche where
  runMessage msg t@(Avalanche attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \loc -> do
        row <- maybe 0 (.row) <$> field LocationPosition loc
        if row >= 1 && row <= 4
          then doStep 1 msg
          else gainSurge attrs
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      withLocationOf iid \loc -> do
        row <- maybe 0 (.row) <$> field LocationPosition loc
        when (row >= 1 && row <= 4) do
          moveTo_ attrs iid (LocationInRow (row - 1))
          doStep 2 msg
      pure t
    DoStep 2 (Revelation iid (isSource attrs -> True)) -> do
      withLocationOf iid \loc -> do
        row <- maybe 0 (.row) <$> field LocationPosition loc
        sid <- getRandom
        beginSkillTest sid iid attrs iid #willpower (Fixed row)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      doStep 1 (Revelation iid (toSource attrs))
      pure t
    _ -> Avalanche <$> liftRunMessage msg attrs
