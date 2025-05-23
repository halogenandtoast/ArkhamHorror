module Arkham.Enemy.Cards.TheConductorBeastFromBeyondTheGate (theConductorBeastFromBeyondTheGate) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Agenda
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement

newtype TheConductorBeastFromBeyondTheGate = TheConductorBeastFromBeyondTheGate EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theConductorBeastFromBeyondTheGate :: EnemyCard TheConductorBeastFromBeyondTheGate
theConductorBeastFromBeyondTheGate =
  enemyWith
    TheConductorBeastFromBeyondTheGate
    Cards.theConductorBeastFromBeyondTheGate
    (3, Static 4, 3)
    (2, 1)
    (spawnAtL ?~ SpawnAt LeftmostLocation)

instance HasAbilities TheConductorBeastFromBeyondTheGate where
  getAbilities (TheConductorBeastFromBeyondTheGate a) = case a.placement of
    AttachedToAgenda {} ->
      [mkAbility a 3 $ SilentForcedAbility $ AgendaAdvances #after AnyAgenda]
    _ ->
      extend
        a
        [ forcedAbility a 1 $ EnemyDefeated #when Anyone ByAny (be a)
        , forcedAbility a 2 $ LocationLeavesPlay #when (locationWithEnemy a)
        ]

instance RunMessage TheConductorBeastFromBeyondTheGate where
  runMessage msg e@(TheConductorBeastFromBeyondTheGate attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      agenda <- getCurrentAgenda
      place attrs $ AttachedToAgenda agenda
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      mLocation <- selectOne $ LocationInDirection RightOf (locationWithEnemy attrs)
      for_ mLocation (enemyMoveTo attrs)
      pure e
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      push $ InvestigatorDrawEnemy iid attrs.id
      pure e
    _ -> TheConductorBeastFromBeyondTheGate <$> liftRunMessage msg attrs
