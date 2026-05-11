module Arkham.Treachery.Cards.RootsOfTheEarth (rootsOfTheEarth) where

import Arkham.Calculation
import Arkham.Location.Cards (theGateOfYquaa)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RootsOfTheEarth = RootsOfTheEarth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rootsOfTheEarth :: TreacheryCard RootsOfTheEarth
rootsOfTheEarth = treachery RootsOfTheEarth Cards.rootsOfTheEarth

instance RunMessage RootsOfTheEarth where
  runMessage msg t@(RootsOfTheEarth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility
        $ SumCalculation
          [ Fixed 2
          , DistanceFromCalculation iid $ locationIs theGateOfYquaa
          ]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> RootsOfTheEarth <$> liftRunMessage msg attrs
