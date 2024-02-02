module Arkham.Event.Cards.HeroicRescue2 (
  heroicRescue2,
  HeroicRescue2 (..),
) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Movement
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype HeroicRescue2 = HeroicRescue2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

heroicRescue2 :: EventCard HeroicRescue2
heroicRescue2 = event HeroicRescue2 Cards.heroicRescue2

instance RunMessage HeroicRescue2 where
  runMessage msg e@(HeroicRescue2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ [(windowType -> Window.EnemyWouldAttack details')] _ | eid == toId attrs -> do
      let iid' = fromJustNote "wrong target" $ preview _InvestigatorTarget (attackTarget details')
      lid <- fieldJust InvestigatorLocation iid'
      mlid <- field InvestigatorLocation iid

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
            [Move $ move attrs iid lid | Just lid /= mlid]
              <> [EnemyAttack (details {attackTarget = toTarget iid})]
              <> [EnemyDamage (attackEnemy details) $ nonAttack attrs 1 | canDealDamage]
          _ -> error "Mismatched"
      pure e
    _ -> HeroicRescue2 <$> runMessage msg attrs
