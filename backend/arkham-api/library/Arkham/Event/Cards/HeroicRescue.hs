module Arkham.Event.Cards.HeroicRescue (
  heroicRescue,
  HeroicRescue (..),
) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype HeroicRescue = HeroicRescue EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heroicRescue :: EventCard HeroicRescue
heroicRescue = event HeroicRescue Cards.heroicRescue

instance RunMessage HeroicRescue where
  runMessage msg e@(HeroicRescue attrs) = case msg of
    InvestigatorPlayEvent iid eid _ [windowType -> Window.EnemyWouldAttack details'] _ | eid == toId attrs -> do
      canDealDamage <- withoutModifier iid CannotDealDamage
      let enemy = attackEnemy details'
      pushAll
        $ EnemyEngageInvestigator enemy iid
        : ChangeEnemyAttackTarget enemy (toTarget iid)
        : [AfterEnemyAttack enemy [EnemyDamage enemy $ nonAttack attrs 1] | canDealDamage]
      pure e
    _ -> HeroicRescue <$> runMessage msg attrs
