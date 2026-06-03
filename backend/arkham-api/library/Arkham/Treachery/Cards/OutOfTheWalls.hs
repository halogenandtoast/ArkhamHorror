module Arkham.Treachery.Cards.OutOfTheWalls (outOfTheWalls) where

import {-# SOURCE #-} Arkham.Game.Utils (maybeEnemyLocation)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Projection
import Arkham.Scenarios.HemlockHouse.Helpers (locationSealCount)
import Arkham.Trait (Trait (Dormant))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OutOfTheWalls = OutOfTheWalls TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outOfTheWalls :: TreacheryCard OutOfTheWalls
outOfTheWalls = treachery OutOfTheWalls Cards.outOfTheWalls

instance RunMessage OutOfTheWalls where
  runMessage msg t@(OutOfTheWalls attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        maybeEnemyLocation lid >>= \case
          Just _ -> assignDamageAndHorror iid attrs 1 1
          Nothing -> do
            seals <- locationSealCount lid
            if seals > 0
              then randomDiscard iid attrs
              else do
                isDormant <- lid <=~> LocationWithTrait Dormant
                when isDormant do
                  card <- field LocationCard lid
                  push $ FlipToEnemyLocation lid card
      pure t
    _ -> OutOfTheWalls <$> liftRunMessage msg attrs
