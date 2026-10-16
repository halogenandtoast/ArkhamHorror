module Arkham.Homebrew.TheLegendOfSleepyHollow.Enemies.DisembodiedSkull (disembodiedSkull) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Types (Field (LocationDoom))
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)
import Arkham.Placement
import Arkham.Projection

newtype DisembodiedSkull = DisembodiedSkull EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disembodiedSkull :: EnemyCard DisembodiedSkull
disembodiedSkull = enemy DisembodiedSkull Cards.disembodiedSkull

{- | "If there is already doom on Disembodied Skull's location, it looses aloof
and gets +2 fight and alert." "Already" is real doom: the skull's own "treated
as if it had doom" (see 'locationIsDoomed') must not feed itself.
-}
instance HasModifiersFor DisembodiedSkull where
  getModifiersFor (DisembodiedSkull a) = case a.placement of
    AtLocation lid -> do
      doom <- field LocationDoom lid
      modifySelfWhen
        a
        (doom > 0)
        [RemoveKeyword Keyword.Aloof, EnemyFight 2, AddKeyword Keyword.Alert]
    _ -> pure mempty

instance HasAbilities DisembodiedSkull where
  getAbilities (DisembodiedSkull a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "disembodiedSkull.story"
      $ skillTestAbility
      $ restricted a 1 (youExist $ investigatorEngagedWith a) #parley

instance RunMessage DisembodiedSkull where
  runMessage msg e@(DisembodiedSkull attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember listenedToAVictimsStory
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> DisembodiedSkull <$> liftRunMessage msg attrs
