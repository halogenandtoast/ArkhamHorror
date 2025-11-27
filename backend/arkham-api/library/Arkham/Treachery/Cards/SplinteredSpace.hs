module Arkham.Treachery.Cards.SplinteredSpace (splinteredSpace) where

import Arkham.Matcher
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Trait (Trait (Hex))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SplinteredSpace = SplinteredSpace TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

splinteredSpace :: TreacheryCard SplinteredSpace
splinteredSpace = treachery SplinteredSpace Cards.splinteredSpace

instance RunMessage SplinteredSpace where
  runMessage msg t@(SplinteredSpace attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      doStep 1 msg
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      n <-
        selectCount $ TreacheryAttachedToLocation (locationWithInvestigator iid) <> TreacheryWithTrait Hex
      assignDamage iid attrs n
      pure t
    DoStep 1 (Revelation _ (isSource attrs -> True)) -> do
      xs <- select $ treacheryIs Cards.splinteredSpace
      when (length xs == 3) $ for_ xs $ toDiscard attrs
      pure t
    _ -> SplinteredSpace <$> liftRunMessage msg attrs
