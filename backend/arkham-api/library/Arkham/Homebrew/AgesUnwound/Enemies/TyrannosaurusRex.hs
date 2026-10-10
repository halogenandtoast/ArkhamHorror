module Arkham.Homebrew.AgesUnwound.Enemies.TyrannosaurusRex (tyrannosaurusRex) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes.HasQueue (HasQueue)
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.SkillTest.Base (SkillTest (skillTestAction, skillTestTarget))
import Control.Monad.Trans.Class (MonadTrans)

newtype TyrannosaurusRex = TyrannosaurusRex EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Huge. Vicious. Hungry./ Massive is on the card def.
tyrannosaurusRex :: EnemyCard TyrannosaurusRex
tyrannosaurusRex = enemy TyrannosaurusRex Cards.tyrannosaurusRex

{- | "Forced - When you would attack or evade Tyrannosaurus Rex: Test [willpower]
(3). If you fail, cancel the triggering attack or evasion attempt. (Limit once
per round.)"

Two abilities, because the engine raises a separate window for each attempt:
'AttemptToFight' from @Enemy/Runner@ and 'AttemptToEvade' from
@Investigator/Runner@. Both windows are checked with the attempt's own skill
test already queued behind them, which is what 'cancelAttempt' reaches for.
-}
instance HasAbilities TyrannosaurusRex where
  getAbilities (TyrannosaurusRex a) =
    extend
      a
      [ playerLimit PerRound $ mkAbility a 1 $ forced $ AttemptToFight #when You (be a)
      , playerLimit PerRound $ mkAbility a 2 $ forced $ AttemptToEvade #when You (be a)
      ]

{- | "cancel the triggering attack or evasion attempt" -- drop the queued attempt
rather than the test the card just made.

The pending fight is the @BeginSkillTest@ built by
@Arkham.Behavior.Fight.mkAttackMessage@ (a Fight test targeting this enemy); the
pending evade is the @TryEvadeEnemy@ that has not become a test yet. Either way
the follow-up "after you fought/evaded" window is dropped with it, so nothing
reacts to an attempt that did not happen.
-}
cancelAttempt :: (MonadTrans t, HasQueue Message m) => EnemyAttrs -> t m ()
cancelAttempt attrs = do
  matchingDon't \case
    BeginSkillTestWithPreMessages' _ st ->
      skillTestAction st `elem` [Just Action.Fight, Just Action.Evade]
        && isTarget attrs (skillTestTarget st)
    TryEvadeEnemy _ _ eid _ _ _ -> eid == attrs.id
    _ -> False
  matchingDon't \case
    AfterEvadeEnemy _ eid -> eid == attrs.id
    _ -> False

instance RunMessage TyrannosaurusRex where
  runMessage msg e@(TyrannosaurusRex attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) n | n `elem` [1, 2] -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability n) attrs #willpower (Fixed 3)
      pure e
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      cancelAttempt attrs
      pure e
    FailedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      cancelAttempt attrs
      pure e
    _ -> TyrannosaurusRex <$> liftRunMessage msg attrs
