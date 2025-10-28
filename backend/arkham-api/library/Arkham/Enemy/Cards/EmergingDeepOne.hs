module Arkham.Enemy.Cards.EmergingDeepOne (emergingDeepOne) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (delayEngagementL)
import Arkham.Helpers.Location (getLocationOf, placementLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), getCombinedModifiers)
import Arkham.Location.FloodLevel
import Arkham.Matcher

newtype EmergingDeepOne = EmergingDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergingDeepOne :: EnemyCard EmergingDeepOne
emergingDeepOne = enemy EmergingDeepOne Cards.emergingDeepOne (3, Static 2, 1) (1, 1)

instance HasAbilities EmergingDeepOne where
  getAbilities (EmergingDeepOne a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after (You <> at_ FloodedLocation) (be a)

instance RunMessage EmergingDeepOne where
  runMessage msg e@(EmergingDeepOne attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      pure $ EmergingDeepOne $ attrs & delayEngagementL .~ True
    EnemySpawn details | details.enemy == attrs.id && enemyDelayEngagement attrs -> do
      let
        go = \case
          SpawnEngagedWith imatcher -> do
            select imatcher >>= \case
              [iid] -> do
                mods <- getCombinedModifiers [toTarget attrs, toTarget attrs.cardId]
                let
                  getModifiedSpawnAt [] = Nothing
                  getModifiedSpawnAt (ChangeSpawnWith iid' m : _) | iid' == iid = Just m
                  getModifiedSpawnAt (_ : xs) = getModifiedSpawnAt xs
                maybe (getLocationOf iid) go (getModifiedSpawnAt mods)
              _ -> pure Nothing
          SpawnAt lm ->
            select lm <&> \case
              [lid] -> Just lid
              _ -> Nothing
          SpawnAtLocation lid -> pure (Just lid)
          SpawnPlaced p -> placementLocation p
          _ -> pure Nothing

      msg' <-
        go details.spawnAt >>= \case
          Nothing -> pure msg
          Just lid ->
            getFloodLevel lid <&> \case
              FullyFlooded -> msg
              _ -> EnemySpawn details {spawnDetailsExhausted = True, spawnDetailsUnengaged = True}
      EmergingDeepOne <$> liftRunMessage msg' attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> EmergingDeepOne <$> liftRunMessage msg attrs
