{- | Act 2, Best Laid Plans.

The culprit is pinned where it stands and the investigators have to put it down
there. "You deal 1 additional damage" is a damage-scoped 'DamageTaken' modifier
rather than a second damage instance, so the extra point belongs to the damage
being dealt -- the window fires before 'getModifiedDamageAmount' reads the
amount, which is what makes the whole hit one hit for defeat purposes.
-}
module Arkham.Homebrew.AndThenThereWereNone.Acts.BestLaidPlans (bestLaidPlans) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Window (damagedEnemy)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher

newtype BestLaidPlans = BestLaidPlans ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bestLaidPlans :: ActCard BestLaidPlans
bestLaidPlans = act (2, A) BestLaidPlans Cards.bestLaidPlans Nothing

-- | "The Culprit enemy cannot move."
instance HasModifiersFor BestLaidPlans where
  getModifiersFor (BestLaidPlans a) = when (onSide A a) do
    modifySelect a culpritEnemy [CannotMove]

instance HasAbilities BestLaidPlans where
  getAbilities = actAbilities \a ->
    scenarioI18n
      [ -- "{reaction} When you would deal 1 or more damage to the Culprit
        -- enemy, if there are no clues on its location: You deal 1 additional
        -- damage."
        withI18nTooltip "bestLaidPlans.additionalDamage"
          $ mkAbility a 1
          $ freeReaction
          $ EnemyWouldTakeDamageWithAmount
            #when
            (SourceUsedBy You)
            (culpritEnemy <> EnemyAt LocationWithoutClues)
            (atLeast 1)
      , -- "Objective - If the Culprit enemy is defeated, advance."
        withI18nTooltip "bestLaidPlans.objective"
          $ mkAbility a 2
          $ Objective
          $ forced
          $ EnemyDefeated #after Anyone ByAny culpritEnemy
      ]

instance RunMessage BestLaidPlans where
  runMessage msg a@(BestLaidPlans attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (damagedEnemy -> eid) _ -> do
      damageModifier (attrs.ability 1) eid (DamageTaken 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> BestLaidPlans <$> liftRunMessage msg attrs
