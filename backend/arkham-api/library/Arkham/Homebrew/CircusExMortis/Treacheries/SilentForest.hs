module Arkham.Homebrew.CircusExMortis.Treacheries.SilentForest (silentForest) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Treacheries.CrashingTrees (forestFailure, forestRevelation)
import Arkham.Treachery.Import.Lifted

newtype SilentForest = SilentForest TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silentForest :: TreacheryCard SilentForest
silentForest = treachery SilentForest Cards.silentForest

instance RunMessage SilentForest where
  runMessage msg t@(SilentForest attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      forestRevelation attrs iid #willpower
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      forestFailure iid "silentForest" n "takeHorror" (assignHorror iid attrs n)
      pure t
    _ -> SilentForest <$> liftRunMessage msg attrs
