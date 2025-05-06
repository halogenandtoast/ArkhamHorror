module Arkham.Treachery.Cards.PrimevalTerror (primevalTerror) where

import Arkham.Distance
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Location.Cards (theGateOfYquaa)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PrimevalTerror = PrimevalTerror TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primevalTerror :: TreacheryCard PrimevalTerror
primevalTerror = treachery PrimevalTerror Cards.primevalTerror

instance RunMessage PrimevalTerror where
  runMessage msg t@(PrimevalTerror attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      Distance distance <-
        fromMaybe (Distance 0) <$> runMaybeT do
          lid <- MaybeT $ getMaybeLocation iid
          gate <- MaybeT $ selectOne $ locationIs theGateOfYquaa
          MaybeT $ getDistance lid gate
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed $ 2 + distance)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> PrimevalTerror <$> liftRunMessage msg attrs
