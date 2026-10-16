module Arkham.Homebrew.TheLegendOfSleepyHollow.Enemies.TheHeadlessHorseman (
  theHeadlessHorseman,
  ignoreTheHorsemansForcedEffect,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)
import Arkham.Message.Lifted.Placement

newtype TheHeadlessHorseman = TheHeadlessHorseman EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeadlessHorseman :: EnemyCard TheHeadlessHorseman
theHeadlessHorseman =
  enemy TheHeadlessHorseman Cards.theHeadlessHorseman
    & setPrey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

{- | "Ignore the forced effect on The Headless Horseman." Act 3 publishes this;
the __Forced__ stays a printed ability and is only narrowed by it.
-}
ignoreTheHorsemansForcedEffect :: ModifierType
ignoreTheHorsemansForcedEffect =
  ScenarioModifier "theLegendOfSleepyHollow.ignoreTheHorsemansForcedEffect"

instance HasAbilities TheHeadlessHorseman where
  getAbilities (TheHeadlessHorseman a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "theHeadlessHorseman.flee"
          $ restricted a 1 (Negate $ SelfHasModifier ignoreTheHorsemansForcedEffect)
          $ forced
          $ EnemyWouldBeDefeated #when (be a)
      , scenarioI18n
          $ withI18nTooltip "theHeadlessHorseman.soul"
          $ mkAbility a 2
          $ triggered (EnemyEvaded #after You (be a)) (HorrorCost (a.ability 2) YouTarget 1)
      ]

instance RunMessage TheHeadlessHorseman where
  runMessage msg e@(TheHeadlessHorseman attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      healAllDamage (attrs.ability 1) attrs
      selectEach (TreacheryIsAttachedTo $ toTarget attrs) (toDiscard (attrs.ability 1))
      place attrs (OutOfPlay SetAsideZone)
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      remember gazedIntoTheHorsemansSoul
      pure e
    _ -> TheHeadlessHorseman <$> liftRunMessage msg attrs
