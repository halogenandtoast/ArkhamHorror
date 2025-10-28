module Arkham.Treachery.Cards.ShatteredAges (shatteredAges) where

import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ShatteredAges = ShatteredAges TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shatteredAges :: TreacheryCard ShatteredAges
shatteredAges = treachery ShatteredAges Cards.shatteredAges

instance RunMessage ShatteredAges where
  runMessage msg t@(ShatteredAges attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      locations <- select $ NotLocation $ locationIs Locations.nexusOfNKai
      for_ locations (placeCluesOn attrs 1)
      pure t
    _ -> ShatteredAges <$> liftRunMessage msg attrs
