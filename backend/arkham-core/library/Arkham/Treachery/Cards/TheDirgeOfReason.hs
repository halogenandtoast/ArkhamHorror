module Arkham.Treachery.Cards.TheDirgeOfReason (theDirgeOfReason, TheDirgeOfReason (..)) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheDirgeOfReason = TheDirgeOfReason TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDirgeOfReason :: TreacheryCard TheDirgeOfReason
theDirgeOfReason = treachery TheDirgeOfReason Cards.theDirgeOfReason

instance RunMessage TheDirgeOfReason where
  runMessage msg t@(TheDirgeOfReason attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      playerClueCount <- field InvestigatorClues iid

      pushWhen (playerClueCount > 0)
        $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) playerClueCount

      when (playerClueCount < 2) do
        assignHorror iid attrs 1
        shuffleIntoDeck iid attrs

      pure t
    _ -> TheDirgeOfReason <$> lift (runMessage msg attrs)
