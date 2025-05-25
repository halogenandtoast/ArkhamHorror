module Arkham.Event.Events.HeroicRescue (heroicRescue) where

import Arkham.Attack
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window

newtype HeroicRescue = HeroicRescue EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heroicRescue :: EventCard HeroicRescue
heroicRescue = event HeroicRescue Cards.heroicRescue

instance RunMessage HeroicRescue where
  runMessage msg e@(HeroicRescue attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let details = getAttackDetails attrs.windows
      let enemy = attackEnemy details
      canDealDamage <- withoutModifier iid CannotDealDamage

      engageEnemy iid enemy
      changeAttackDetails enemy
        $ details
          { attackTarget = SingleAttackTarget (toTarget iid)
          , attackAfter =
              attackAfter details <> [EnemyDamage enemy $ nonAttack (Just iid) attrs 1 | canDealDamage]
          }

      pure e
    _ -> HeroicRescue <$> liftRunMessage msg attrs
