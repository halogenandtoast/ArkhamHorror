{- | "Misshapen Beast gains retaliate and alert during attack and evasion actions
printed on assets or events." Constant text with no printed ability, so it is a
'HasModifiersFor' scoped to the live skill test.

Both keywords are read with 'getModifiedKeywords' from the @Failed (Fight|Evade)@
handler, which is pushed from @FailedSkillTest@ -- the test is still live there,
so scoping on it reaches the retaliate/alert checks.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Enemies.MisshapenBeast (misshapenBeast) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (getSkillTest, skillTestMatches)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype MisshapenBeast = MisshapenBeast EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

misshapenBeast :: EnemyCard MisshapenBeast
misshapenBeast = enemy MisshapenBeast Cards.misshapenBeast

instance HasModifiersFor MisshapenBeast where
  getModifiersFor (MisshapenBeast a) =
    fromMaybe mempty <$> runMaybeT do
      st <- MaybeT getSkillTest
      liftGuardM $ skillTestMatches st.investigator (toSource a) st wary
      lift $ modifySelf a [AddKeyword Keyword.Retaliate, AddKeyword Keyword.Alert]
   where
    -- The basic fight and evade actions are sourced on the enemy itself, so only an
    -- ability belonging to a player's asset or event answers here.
    fromAssetOrEvent = SourceMatchesAny [SourceIsAsset AnyAsset, SourceIsEvent AnyEvent]
    wary =
      SkillTestSourceMatches fromAssetOrEvent
        <> SkillTestOneOf [WhileAttackingAnEnemy (be a), WhileEvadingAnEnemy (be a)]
