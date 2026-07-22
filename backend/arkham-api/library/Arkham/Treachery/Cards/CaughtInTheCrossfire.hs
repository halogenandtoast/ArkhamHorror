module Arkham.Treachery.Cards.CaughtInTheCrossfire (caughtInTheCrossfire) where

import Arkham.Ability
import Arkham.Classes.HasQueue (popMessageMatching)
import Arkham.DamageEffect (DamageAssignment)
import Arkham.Helpers.Window (damagedEnemy)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Trait (Trait (Criminal))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CaughtInTheCrossfire = CaughtInTheCrossfire TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caughtInTheCrossfire :: TreacheryCard CaughtInTheCrossfire
caughtInTheCrossfire = treachery CaughtInTheCrossfire Cards.caughtInTheCrossfire

instance HasAbilities CaughtInTheCrossfire where
  getAbilities (CaughtInTheCrossfire a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ EnemyDealtDamage #when AnyDamageEffect (EnemyWithTrait Criminal <> EnemyAt YourLocation) AnySource
    ]

instance RunMessage CaughtInTheCrossfire where
  runMessage msg t@(CaughtInTheCrossfire attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (damagedEnemy -> eid) _ -> do
      sid <- getRandom
      pendingDamage <-
        lift
          $ popMessageMatching
          $ \case Damaged (EnemyTarget eid') _ -> eid == eid'; _ -> False
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#intellect, #agility] (Fixed 3)
      let damage = pendingDamage >>= \case Damaged _ assignment -> Just assignment; _ -> Nothing
      pure $ CaughtInTheCrossfire $ attrs & setMeta ((eid,) <$> damage)
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      for_ (toResultDefault @(Maybe (EnemyId, DamageAssignment)) Nothing attrs.meta) \(eid, damage) -> do
        damageModifier (attrs.ability 1) eid (DamageTaken (-1))
        push $ Damaged (EnemyTarget eid) damage
      directDamage iid (attrs.ability 1) 1
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      for_ (toResultDefault @(Maybe (EnemyId, DamageAssignment)) Nothing attrs.meta) \(eid, damage) ->
        push $ Damaged (EnemyTarget eid) damage
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> CaughtInTheCrossfire <$> liftRunMessage msg attrs
