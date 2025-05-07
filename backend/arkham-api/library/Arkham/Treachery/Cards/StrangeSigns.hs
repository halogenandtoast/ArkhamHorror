module Arkham.Treachery.Cards.StrangeSigns (strangeSigns) where

import Arkham.Helpers.Location
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StrangeSigns = StrangeSigns TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSigns :: TreacheryCard StrangeSigns
strangeSigns = treachery StrangeSigns Cards.strangeSigns

instance RunMessage StrangeSigns where
  runMessage msg t@(StrangeSigns attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        playerCount <- getPlayerCount
        placeClues attrs lid $ if playerCount == 3 || playerCount == 4 then 2 else 1
      pure t
    _ -> StrangeSigns <$> liftRunMessage msg attrs
