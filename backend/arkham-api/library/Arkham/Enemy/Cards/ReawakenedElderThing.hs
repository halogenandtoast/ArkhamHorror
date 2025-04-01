module Arkham.Enemy.Cards.ReawakenedElderThing (reawakenedElderThing) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..), keysL)
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection

newtype ReawakenedElderThing = ReawakenedElderThing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reawakenedElderThing :: EnemyCard ReawakenedElderThing
reawakenedElderThing = enemy ReawakenedElderThing Cards.reawakenedElderThing (2, Static 3, 1) (1, 1)

instance HasAbilities ReawakenedElderThing where
  getAbilities (ReawakenedElderThing a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyEngaged #when (You <> InvestigatorWithAnyKey) (be a)
      , mkAbility a 2 $ forced $ TakeControlOfKey #when (at_ $ locationWithEnemy a) #any
      ]

instance RunMessage ReawakenedElderThing where
  runMessage msg e@(ReawakenedElderThing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ks <- field InvestigatorKeys iid
      chooseOrRunOneM iid do
        for_ ks \k -> labeled ("Place " <> keyName k) (placeKey attrs k)
      pure e
    EnemyDefeated eid _ _ _ | eid == attrs.id -> do
      mloc <- field EnemyLocation attrs.id
      for_ mloc $ for_ (enemyKeys attrs) . placeKey
      ReawakenedElderThing <$> liftRunMessage msg (attrs & keysL .~ mempty)
    RemovedFromPlay (isSource attrs -> True) -> do
      mloc <- field EnemyLocation attrs.id
      for_ mloc $ for_ (enemyKeys attrs) . placeKey
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      unless attrs.ready $ readyThis attrs
      case attrs.placement of
        InThreatArea iid' | iid == iid' -> pure ()
        _ -> engageEnemy iid attrs
      initiateEnemyAttack attrs (attrs.ability 2) iid

      pure e
    _ -> ReawakenedElderThing <$> liftRunMessage msg attrs
