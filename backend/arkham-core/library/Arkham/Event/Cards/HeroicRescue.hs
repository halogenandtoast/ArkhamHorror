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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

heroicRescue :: EventCard HeroicRescue
heroicRescue = event HeroicRescue Cards.heroicRescue

instance RunMessage HeroicRescue where
  runMessage msg e@(HeroicRescue attrs) = case msg of
    InvestigatorPlayEvent iid eid _ [(windowType -> Window.EnemyWouldAttack details')] _ | eid == toId attrs -> do
      canDealDamage <- withoutModifier iid CannotDealDamage
      popMessageMatching_ \case
        CheckWindow _ windows -> flip
          any
          windows
          \case
            (windowType -> Window.EnemyAttacks details) -> details == details'
            _ -> False
        _ -> False
      popMessageMatching_ \case
        After (PerformEnemyAttack details) -> details == details'
        _ -> False
      replaceMessageMatching
        \case
          PerformEnemyAttack details -> details == details'
          _ -> False
        \case
          PerformEnemyAttack details ->
            EnemyAttack (details {attackTarget = toTarget iid})
              : [EnemyDamage (attackEnemy details) $ nonAttack attrs 1 | canDealDamage]
          _ -> error "Mismatched"
      pure e
    _ -> HeroicRescue <$> runMessage msg attrs
