module Arkham.Treachery.Cards.TheMadnessWithin (theMadnessWithin) where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Helpers.Shuffle
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheMadnessWithin = TheMadnessWithin TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMadnessWithin :: TreacheryCard TheMadnessWithin
theMadnessWithin = treachery TheMadnessWithin Cards.theMadnessWithin

instance RunMessage TheMadnessWithin where
  runMessage msg t@(TheMadnessWithin attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      cards <- getTekelili n
      getCanShuffleIn iid cards >>= \case
        True -> do
          addTekelili iid cards
          let horror = n - length cards
          when (horror > 0) $ assignHorror iid attrs horror
        False -> assignHorror iid attrs n
      pure t
    _ -> TheMadnessWithin <$> liftRunMessage msg attrs
