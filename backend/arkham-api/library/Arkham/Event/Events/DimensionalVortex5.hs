module Arkham.Event.Events.DimensionalVortex5 (dimensionalVortex5, dimensionalVortex5Effect) where

import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Zone

newtype DimensionalVortex5 = DimensionalVortex5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalVortex5 :: EventCard DimensionalVortex5
dimensionalVortex5 = event DimensionalVortex5 Cards.dimensionalVortex5

instance RunMessage DimensionalVortex5 where
  runMessage msg e@(DimensionalVortex5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      locations <- select $ LocationWithEnemy NonEliteEnemy
      chooseTargetM iid locations (handleTarget iid attrs)
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (LocationTarget lid) -> do
      enemies <- select $ EnemyAt $ LocationWithId lid
      for_ enemies \enemy -> do
        place enemy (OutOfPlay SetAsideZone)
        createCardEffect Cards.dimensionalVortex5 (effectMetaTarget lid) attrs enemy
      pure e
    _ -> DimensionalVortex5 <$> liftRunMessage msg attrs

newtype DimensionalVortex5Effect = DimensionalVortex5Effect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalVortex5Effect :: EffectArgs -> DimensionalVortex5Effect
dimensionalVortex5Effect = cardEffect DimensionalVortex5Effect Cards.dimensionalVortex5

instance RunMessage DimensionalVortex5Effect where
  runMessage msg e@(DimensionalVortex5Effect attrs) = runQueueT $ case msg of
    EndRound -> do
      for_ attrs.target.enemy \enemy -> do
        for_ attrs.metaTarget.location \lid -> do
          locationExists <- selectAny $ LocationWithId lid
          if locationExists
            then do
              exhaustThis enemy
              place enemy (AtLocation lid)
            else toDiscard GameSource enemy
      disableReturn e
    _ -> DimensionalVortex5Effect <$> liftRunMessage msg attrs
