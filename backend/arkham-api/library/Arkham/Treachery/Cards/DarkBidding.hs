module Arkham.Treachery.Cards.DarkBidding (darkBidding) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (EnemyAttacks)

newtype DarkBidding = DarkBidding TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkBidding :: TreacheryCard DarkBidding
darkBidding = treachery DarkBidding Cards.darkBidding

instance HasAbilities DarkBidding where
  getAbilities (DarkBidding attrs) = case attrs.placement of
    AttachedToEnemy eid ->
      [ limitedAbility (MaxPer Cards.darkBidding PerAttack 1)
          $ mkAbility attrs 1
          $ forced
          $ EnemyAttacks #when You AnyEnemyAttack (be eid)
      ]
    _ -> []

instance RunMessage DarkBidding where
  runMessage msg t@(DarkBidding attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      mHuntingHorror <- getHuntingHorror
      for_ mHuntingHorror (attachTreachery attrs)
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached.enemy \eid -> do
        enemyAttackModifiers (attrs.ability 1) eid [DamageDealt 1, HorrorDealt 1]
        healDamage eid (attrs.ability 1) 2
      toDiscard (attrs.ability 1) attrs
      pure t
    PlaceEnemyOutOfPlay VoidZone eid | EnemyTarget eid `elem` attrs.attached -> pure t
    _ -> DarkBidding <$> liftRunMessage msg attrs
