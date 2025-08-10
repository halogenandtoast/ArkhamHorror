module Arkham.Event.Events.HeroicRescue2 (heroicRescue2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers
import Arkham.Message.Lifted.Move
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype HeroicRescue2 = HeroicRescue2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heroicRescue2 :: EventCard HeroicRescue2
heroicRescue2 = event HeroicRescue2 Cards.heroicRescue2

instance RunMessage HeroicRescue2 where
  runMessage msg e@(HeroicRescue2 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ [windowType -> Window.EnemyWouldAttack details'] _ | eid == toId attrs -> do
      let iid' = fromJustNote "wrong target" $ preview _InvestigatorTarget =<< details'.singleTarget
      let enemy = details'.enemy

      canDealDamage <- withoutModifier iid CannotDealDamage
      withLocationOf iid' \lid' -> do
        withLocationOf iid \lid -> do
          when (lid /= lid') $ moveTo attrs iid lid'
      enemyEngageInvestigator enemy iid
      push $ ChangeEnemyAttackTarget enemy (toTarget iid)

      when canDealDamage do
        afterEnemyAttack enemy $ nonAttackEnemyDamage (Just iid) attrs 1 enemy
      pure e
    _ -> HeroicRescue2 <$> liftRunMessage msg attrs
