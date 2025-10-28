module Arkham.Treachery.Cards.FromTheOtherSide (fromTheOtherSide) where

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FromTheOtherSide = FromTheOtherSide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fromTheOtherSide :: TreacheryCard FromTheOtherSide
fromTheOtherSide = treachery FromTheOtherSide Cards.fromTheOtherSide

instance RunMessage FromTheOtherSide where
  runMessage msg t@(FromTheOtherSide attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      withLocationOf iid \loc -> do
        whenM (matches loc HauntedLocation) do
          runHauntedAbilities iid
          when (n >= 3) $ runHauntedAbilities iid
      pure t
    _ -> FromTheOtherSide <$> liftRunMessage msg attrs
